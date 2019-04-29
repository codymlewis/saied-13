#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2018-12-28
# Description:
# A simulation of the trust model described in
# http://people.cs.vt.edu/~irchen/5984/pdf/Saied-CS14.pdf

library("Rcpp")

source("TrustManager.r")
source(sprintf("%sAttacks.r", ROOT))
source(sprintf("%sTrustModel.r", ROOT))
source("../Functions.r")

# Find the trust values of the proxies
compute_trust <- function(network, R, w, client_id) {
    total_nodes = nrow(w)
    id = 1:total_nodes
    trust = rep(0, total_nodes)
    for(i in id) {
        trust[i] = calculate_trust(total_nodes, w, network$latest_qrs, R[, , NOTE_INDEX], TRUE, client_id)
    }
    return(data.frame(
        id = 1:total_nodes,
        trust = calculate_trust(total_nodes, w, network$latest_qrs,
                                R[, , NOTE_INDEX])
    ))
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
            restrict_reports(R[i, ,], c_target, s_target, C_MAX, S_MAX, eta,
                             SERVICE_INDEX, CAPABILITY_INDEX, NOTE_INDEX)
        }
    )
    d = matrix(distances, nrow = total_nodes, ncol = total_nodes, byrow = TRUE)
    rm(distances)
    weights = sapply(1:total_nodes,
        function(i) {
            weigh_reports(lambda, theta, R[i, ,], d[i, ], time, NOTE_INDEX,
                          TIME_INDEX, TRUE, client)
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

# Select suitable entities for a target service
entity_selection <- function(network, lambda, theta, eta,
                             R, c_target, s_target, time, client) {
    total_nodes = length(R[, 1, 1])
    distances = sapply(1:total_nodes,
        function(i) {
            restrict_reports(R[i, ,], c_target, s_target, C_MAX, S_MAX, eta,
                             SERVICE_INDEX, CAPABILITY_INDEX, NOTE_INDEX)
        }
    )
    d = matrix(distances, nrow = total_nodes, ncol = total_nodes, byrow = TRUE)
    rm(distances)
    weights = sapply(1:length(R[, 1, 1]),
        function(i) {
            weigh_reports(lambda, theta, R[i, ,], d[i, ], time, NOTE_INDEX,
                              TIME_INDEX, TRUE, client)
        }
    )
    w = matrix(weights, nrow = total_nodes, ncol = total_nodes, byrow = TRUE)
    rm(weights)
    T = compute_trust(network, R, w, client)
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
    trusted_ids = trusted_ids[!trusted_ids %in% network$ill_reputed_nodes]
    return(list(T$trust, trusted_ids[!trusted_ids %in% client], nodemon_data))
}


# Run some post initialization operations
post_init <- function(network, lambda, theta, eta, R,
                      time, total_nodes, cs_targets, phase) {
    well_reputed_nodes = network$id[!network$id %in% network$ill_reputed_node]
    client = well_reputed_nodes[floor(runif(1, min=1, max=length(well_reputed_nodes)))]
    es_result = entity_selection(network, lambda, theta, eta, R,
                              cs_targets[[1]], cs_targets[[2]], time, client)
    trust_values = es_result[[1]]
    for(i in 1:total_nodes) {
        network$trust[i, phase] = trust_values[[i]]
    }
    server = es_result[[2]][[1]]
    if(length(well_reputed_nodes) == 0) {
        return(list(R, network, NA))
    }
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
                             S_MAX, C_MAX, poor_witnesses, constrained, phases)
    network$attack_type = assign_attack_types(network$attack_type, malicious_percent,
                                  total_nodes, attack_type)
    R = create_report_set(total_nodes)
    nodemon_data = create_nodemon_matrix(phases)
    end_phases = phases
    for(i in 1:phases) {
        cat_progress(i, phases, prefix=sprintf("%d/%d transactions", i, phases))
        R = initialize(network, R, time, lambda, theta, eta)
        if((i %% 30) == 0) {
            time = time + 1
        }
        cs_targets = c(floor(runif(1, min=1, max=C_MAX - 1)), get_random_service())
        result = post_init(network, lambda, theta, eta, R,
                           time, total_nodes, cs_targets, i)
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
    attack_name = get_attack_name(attack_type)
    dir.create(sprintf("./graphs/%s", REPUTATION_THRESHOLD),
                       showWarnings=FALSE)
    dir.create(sprintf("./graphs/%s/%s", REPUTATION_THRESHOLD, attack_name),
               showWarnings=FALSE)
    dir.create(sprintf("./graphs/%s/%s/%s", REPUTATION_THRESHOLD, attack_name,
                       folder), showWarnings=FALSE)
    graph_target_group_trust(network, end_phases)
    ggsave(file = sprintf("./graphs/%s/%s/%s/Trust_Groups.png", REPUTATION_THRESHOLD, attack_name, folder))
}
