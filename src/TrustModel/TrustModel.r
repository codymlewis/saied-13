#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2018-12-28
# Description:
# A simulation of the trust model described in
# http://people.cs.vt.edu/~irchen/5984/pdf/Saied-CS14.pdf

library("Rcpp")

sourceCpp("HandleReports.cpp")
source("Attacks.r")
source("TrustManager.r")

RESTRICTED_REPORT <- -1 # Marker showing that the report is restricted
REPUTATION_THRESHOLD <- -1  # Point where a node is so ill reputed that it is
                           # no longer interacted with, in the network
S_MAX = 101 # Max values for the service and capabilities
C_MAX = 101

# Develop a collection of reports on the network
initialize <- function(network, R, time, lambda, theta, eta) {
    node_indices = 1:length(network$service)
    ill_reputed = node_indices %in% network$ill_reputed_nodes
    for(i in node_indices) {
        if(!ill_reputed[[i]]) {
            for(j in node_indices) {
                if(!ill_reputed[[j]] && i != j) {
                    s_target = get_random_service()
                    c_target = floor(runif(1, 1, C_MAX))
                    R[i, j,] = transaction(
                        network$service[[i]],
                        network$capability[[i]],
                        c_target,
                        s_target,
                        network$accurate_note_take[[j]],
                        time,
                        network$malicious[[j]],
                        network$attack_type[[j]],
                        network$recommendations_count[[j]]
                    )
                    network$recommendations_count[[j]] =
                        network$recommendations_count[[j]] + 1
                }
            }
        }
    }
    rm(node_indices)
    rm(ill_reputed)
    return(R)
}

find_s <- function(note_j) {
    return((1 / 2) * (note_j**2 - note_j))
}

# Give the reports a weight based on how recent they were
weigh_reports <- function(lambda, theta, node_reports, report_distances, time) {
    return(sapply(1:length(node_reports[, SERVICE_INDEX]),
        function(j) {
            theta_exp = ((find_s(node_reports[j, NOTE_INDEX]) + 1) *
                         (time - node_reports[j, TIME_INDEX]))
            (lambda ** report_distances[j]) * (theta ** theta_exp)
        }
    ))
}

# Find the trust values of the proxies
compute_trust <- function(network, R, w) {
    total_nodes = nrow(w)
    return(data.frame(
        id = 1:total_nodes,
        trust = sapply(1:total_nodes,
            function(i) {
                numerator = 0
                denominator = 0
                for(j in 1:total_nodes) {
                    if(w[i, j] != RESTRICTED_REPORT) {
                        numerator = numerator + (
                            w[i, j] *
                            network$QR[[j]][[1]] *
                            R[i, j, NOTE_INDEX]
                        )
                        denominator = denominator + w[i, j]
                    }
                }
                `if`(denominator == 0, 0, numerator / denominator)
            }
        )
    ))
}

# Select suitable entities for a target service
entity_selection <- function(network, lambda, theta, eta,
                             R, c_target, s_target, time) {
    total_nodes = length(R[, 1, 1])
    distances = sapply(1:total_nodes,
        function(i) {
            restrict_reports(R[i, ,], c_target, s_target, C_MAX, S_MAX, eta,
                             SERVICE_INDEX, CAPABILITY_INDEX, NOTE_INDEX)
        }
    )
    d = matrix(distances, nrow = total_nodes, ncol = total_nodes, byrow = TRUE)
    rm(distances)
    restricted_reports = d == RESTRICTED_REPORT
    weights = sapply(1:length(R[, 1, 1]),
        function(i) {
            ifelse(d[i, ] == RESTRICTED_REPORT,
                RESTRICTED_REPORT,
                weigh_reports(lambda, theta, R[i, ,], d[i, ], time)
            )
        }
    )
    w = matrix(weights, nrow = total_nodes, ncol = total_nodes, byrow = TRUE)
    rm(weights)
    T = compute_trust(network, R, w)
    rm(d)
    rm(w)
    nodemon_data = c(
        mean(R[NODE_MON_ID, , SERVICE_INDEX]),
        mean(R[NODE_MON_ID, , CAPABILITY_INDEX]),
        mean(R[NODE_MON_ID, , NOTE_INDEX]),
        mean(time - R[NODE_MON_ID, , TIME_INDEX]),
        T$trust[[NODE_MON_ID]]
    )
    trusted_ids = T[order(-T$trust),]$id
    return(list(T$trust, trusted_ids[!trusted_ids %in% network$ill_reputed_nodes], nodemon_data))
}

# Give a value stating the significance of older occurances
find_c_i <- function(theta, t_1, t_i) {
    return(theta ** (t_1 - t_i))
}

# Update the quality of recommendation of nodes that made reports on the server
# simultaneously calculates the reputation of the server
update_qrs <- function(network, R, w, client, server, client_note, theta, time) {
    for(j in 1:length(R[server, , SERVICE_INDEX])) {
        if(w[server, j] != RESTRICTED_REPORT) {
            X = j # To make the equations look like those on the paper
            C_F = w[server, X] * network$QR[[client]][[1]]
            QRXF = C_F * (-abs(R[server, X, NOTE_INDEX] - client_note))
            numerator=denominator=0
            numerator = sum(sapply(1:length(network$QR[[X]]),
                function(i) {
                    c_i = find_c_i(theta, network$time_QR[[X]][1],
                                   network$time_QR[[X]][i])
                    c_i * network$QR[[X]][[i]] + QRXF
                }
            ))
            denominator = sum(sapply(1:length(network$QR[[X]]),
                function(i) {
                    c_i = find_c_i(theta, network$time_QR[[X]][1],
                                   network$time_QR[[X]][i])
                    c_i + abs(C_F)
                }
            ))
            network$QR[[X]] = c(
                `if`(denominator == 0,
                    0,
                    numerator / denominator),
                network$QR[[X]]
            )
            if(network$QR[[X]][[1]] < -1) {
                network$QR[[X]][[1]] = -1
            } else if(network$QR[[X]][[1]] > 1) {
                network$QR[[X]][[1]] = 1
            }
            network$time_QR[[X]] = c(time, network$time_QR[[X]])
        }
    }
    # Update reputation of the server
    times_been_server = length(network$clients[[server]]) + 1
    network$client_notes[[server]][times_been_server] = client_note
    network$clients[[server]][times_been_server] = client
    network$client_QRs[[server]][times_been_server] = head(network$QR[[client]], 1)
    network$client_time_QRs[[server]][times_been_server] = head(network$time_QR[[client]], 1)
    network$reputation[[server]] = calculate_reputation(network, server, theta)
    if(network$reputation[[server]] < REPUTATION_THRESHOLD) {
        network$ill_reputed_nodes[[length(network$ill_reputed_nodes) + 1]] =
            server
    }
    return(network)
}

# Calculate the reputation of a server
calculate_reputation <- function(network, server, theta) {
    sum = 0
    for(j in seq(2, length(network$client_notes[[server]]))) {
        client = network$clients[[server]][[j]]
        sum = sum +
            find_c_i(theta, network$time_QR[[client]][1],
                     network$client_time_QR[[server]][[j]]) *
            network$client_notes[[server]][[j]] *
            network$client_QRs[[server]][[j]]
    }
    return(sum)
}

# Simulate a transaction used at the initialization phase,
# add a report entry based on that
transaction <- function(server_service, server_capability,
                        capability_target, service_target, accurate_note_take,
                        time, client_is_malicious, client_attack_type,
                        client_rec_count) {
    report = rep(0, 4)
    if(client_attack_type && client_attack_type %% SERVICE_SET_FLAG == 0) {
        report[SERVICE_INDEX] = service_set()
    } else {
        report[SERVICE_INDEX] = service_target
    }
    if(client_attack_type && client_attack_type %% CAPABILITY_SET_FLAG == 0) {
        report[CAPABILITY_INDEX] = capability_set()
    } else {
        report[CAPABILITY_INDEX] = server_capability
    }
    if(client_attack_type > NO_ATTACK_FLAG) {
        if(client_attack_type %% BAD_MOUTH_FLAG == 0) {
            report[NOTE_INDEX] = bad_mouth()
        } else if(client_attack_type %% GOOD_MOUTH_FLAG == 0) {
            report[NOTE_INDEX] = good_mouth()
        } else if(client_attack_type %% ON_OFF_FLAG == 0) {
            report[NOTE_INDEX] = on_off(
                (floor(client_rec_count / ON_OFF_TOGGLE) %% 2) == 1
            )
        } else { # Action performed with context attacks
            report[NOTE_INDEX] = take_note(
                report[SERVICE_INDEX], report[CAPABILITY_INDEX],
                service_target, capability_target
            )
        }
    } else {
        note = take_note(report[SERVICE_INDEX], report[CAPABILITY_INDEX],
                         service_target, capability_target)
        report[NOTE_INDEX] = `if`(
            runif(1) < accurate_note_take,
            note,
            wrong_note(note)
        )
    }
    if(client_attack_type && client_attack_type %% TIME_DECAY_FLAG == 0) {
        report[TIME_INDEX] = time_decay(time)
    } else {
        report[TIME_INDEX] = time
    }
    return(report)
}

# Return the note value based on how a proxy will perform on a transaction
take_note <- function(server_service, server_capability, service_target, capability_target) {
    if(server_service < service_target) {
        return(-1)
    } else if(server_service > service_target &&
                server_capability >= capability_target) {
        return(1)
    } else {
        return(0)
    }
}

# Return a note other than the one specified
wrong_note <- function(note) {
    wrong_vals = setdiff(c(-1, 0, 1), note)
    return(`if`(runif(1) < 0.5, wrong_vals[1], wrong_vals[2]))
}

# Perform a transaction and update the values stored in the Trust Manager
transaction_and_update <- function(network, R, time, lambda, theta, eta,
                                   client, server, c_target, s_target) {
    R[server, client,] = transaction(
        network$service[server],
        network$capability[server],
        c_target,
        s_target,
        network$accurate_note_take[[client]],
        time,
        network$malicious[[client]],
        network$attack_type[[client]],
        network$recommendations_count[[client]]
    )
    network$recommendations_count[[client]] = network$recommendations_count[[client]] + 1
    total_nodes = length(R[, 1, 1])
    distances = sapply(1:length(R[, 1, 1]),
        function(i) {
            `if`(is.null(R[i, , SERVICE_INDEX]),
                RESTRICTED_REPORT,
                restrict_reports(R[i, ,], c_target, s_target, C_MAX, S_MAX, eta,
                                 SERVICE_INDEX, CAPABILITY_INDEX, NOTE_INDEX))
        }
    )
    d = matrix(distances, nrow = total_nodes, ncol = total_nodes, byrow = TRUE)
    rm(distances)
    weights = sapply(1:total_nodes,
        function(i) {
            ifelse(d[i, ] == RESTRICTED_REPORT,
                RESTRICTED_REPORT,
                weigh_reports(lambda, theta, R[i, ,], d[i, ], time)
            )
        }
    )
    w = matrix(weights, nrow = total_nodes, ncol = total_nodes, byrow = TRUE)
    rm(weights)
    network = update_qrs(network, R, w, client, server,
                         R[server, client, NOTE_INDEX], theta, time)
    rm(d)
    rm(w)
    return(list(R, network))
}

# Run some post initialization operations
post_init <- function(network, lambda, theta, eta, R,
                      time, total_nodes, cs_targets) {
    es_result = entity_selection(network, lambda, theta, eta, R,
                              C_MAX - 1, cs_targets[[2]], time)
    network$final_trust = es_result[[1]]
    server = es_result[[2]][1]
    client = server
    well_reputed_nodes = network$id[!network$id %in% network$ill_reputed_node]
    well_reputed_nodes = well_reputed_nodes[!well_reputed_nodes %in% server]
    if(length(well_reputed_nodes) == 0) {
        return(list(R, network, NA))
    }
    client = well_reputed_nodes[
        floor(runif(1, min=1, max=length(well_reputed_nodes)))
    ]
    result = transaction_and_update(network, R, time,
                                    lambda, theta, eta,
                                    client, server,
                                    network$capability[[server]],
                                    cs_targets[[2]])
    R = result[[1]]
    network = result[[2]]
    return(list(R, network, es_result[[3]]))
}

# Run through the system operations
run <- function(lambda, theta, eta, total_nodes, malicious_percent,
                phases, folder, attack_type, poor_witnesses, constrained) {
    time = 1
    network = create_network(total_nodes, malicious_percent, time,
                             S_MAX, C_MAX, poor_witnesses, constrained)
    network$attack_type = assign_attack_types(network$attack_type, malicious_percent,
                                  total_nodes, attack_type)
    R = create_report_set(total_nodes)
    nodemon_data = create_nodemon_matrix(phases)
    end_phases = phases
    for(i in 1:phases) {
        cat(sprintf("Transaction: %d\n", i))
        R = initialize(network, R, time, lambda, theta, eta)
        if((i %% 30) == 0) {
            time = time + 1
        }
        cs_targets = c(floor(runif(1, min=1, max=C_MAX - 1)), get_random_service())
        result = post_init(network, lambda, theta, eta, R,
                           time, total_nodes, cs_targets)
        R = result[[1]]
        network = result[[2]]
        if(is.na(result[[3]])) {
            end_phases = i
            break
        }
        nodemon_data[i, ] = result[[3]]
    }
    print("Ill Reputed Nodes")
    print(network$ill_reputed_nodes)
    # graph_node_data(total_nodes, network, folder)
    attack_name = get_attack_name(attack_type)
    dir.create(sprintf("./graphs/%s", REPUTATION_THRESHOLD),
                       showWarnings=FALSE)
    dir.create(sprintf("./graphs/%s/%s", REPUTATION_THRESHOLD, attack_name),
               showWarnings=FALSE)
    dir.create(sprintf("./graphs/%s/%s/%s", REPUTATION_THRESHOLD, attack_name,
                       folder), showWarnings=FALSE)
    png(file = sprintf("./graphs/%s/%s/%s/Nodemon.png",
                       REPUTATION_THRESHOLD, attack_name, folder))
    graph_nodemon_data(nodemon_data, NODE_MON_ID, network$malicious[[NODE_MON_ID]])
    dev.off()
    png(file = sprintf("./graphs/%s/%s/%s/Node_%s_qr_changes.png",
                       REPUTATION_THRESHOLD, attack_name, folder,
                       NODE_MON_ID))
    graph_single_node(network, NODE_MON_ID)
    dev.off()
    png(file = sprintf("./graphs/%s/%s/%s/Reputations.png",
                       REPUTATION_THRESHOLD, attack_name, folder))
    graph_reputations(network)
    text(
        length(network$id) / 2,
        -1.5,
        sprintf("Only reached transaction %d of %d", end_phases, phases),
        cex = 0.8
    )
    dev.off()
    png(file = sprintf("./graphs/%s/%s/%s/Final_QRs.png",
                       REPUTATION_THRESHOLD, attack_name, folder))
    graph_final_qrs(network)
    dev.off()
    png(file = sprintf("./graphs/%s/%s/%s/Final_Trust.png",
                       REPUTATION_THRESHOLD, attack_name, folder))
    graph_final_trust(network)
    dev.off()
}
