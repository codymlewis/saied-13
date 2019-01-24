#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2018-12-28
# Description:
# A simulation of the trust model described in
# http://people.cs.vt.edu/~irchen/5984/pdf/Saied-CS14.pdf

source("Attacks.r")
source("TrustManager.r")

RESTRICTED_REPORT <- -1 # Marker showing that the report is restricted
REPUTATION_THRESHOLD <- -20 # Point where a node is so ill reputed that it is
                           # no longer interacted with, in the network
S_MAX = 101
C_MAX = 101

# Develop a collection of reports on the network
initialize <- function(network, R, time, lambda, theta, eta, cs_targets) {
    node_indices = 1:length(network$service)
    ill_reputed = node_indices %in% network$ill_reputed_nodes
    for(i in node_indices) {
	if(!ill_reputed[[i]]) {
	    for(j in node_indices) {
	    	if(!ill_reputed[[j]]) {
		    R[i, j,] = transaction(
			network$service[[i]],
			network$capability[[i]],
			cs_targets[[1]],
			cs_targets[[2]],
			network$R_QR[[j]],
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
    R
}

# Find the absolute value of the difference of the 2 values
find_dist <- function(target, current) {
    abs(target - current)
}

# Find the distance between a report's context and a target context
report_dist <- function(node_reports, c_target, s_target, eta,
			dS_max_sq, dC_max_sq, S_max, C_max, j) {
    shared_term = sqrt(
	(dS_max_sq + dC_max_sq) *
	(
	 (find_dist(s_target, node_reports[j, SERVICE_INDEX])**2 /
	  dS_max_sq) +
	     (find_dist(c_target, node_reports[j, CAPABILITY_INDEX])**2 /
	      dC_max_sq)
	)
    )
    unique_term = `if`(
	node_reports[j, NOTE_INDEX] >= 0,
	sqrt(
	    (dS_max_sq + dC_max_sq) *
	    (
	     ((S_max - node_reports[j, SERVICE_INDEX]) /
	      (S_max - (s_target - eta)))**2 +
	     (node_reports[j, CAPABILITY_INDEX] /
	      (c_target + eta))**2
	    )
	),
	sqrt(
	    (dS_max_sq + dC_max_sq) *
	    (
	     ((C_max - node_reports[j, CAPABILITY_INDEX]) /
	      (C_max - (c_target - eta)))**2 +
	     (node_reports[j, SERVICE_INDEX] / (s_target + eta))**2
	    )
	)
    )
    min(shared_term, unique_term)
}

# Find the distance of between a nodes reports and the target conditions
restrict_reports <- function(node_reports, c_target, s_target, eta) {
    dS_max_sq = find_dist(s_target, S_MAX)**2
    dC_max_sq = find_dist(c_target, C_MAX)**2
    t = sqrt(dS_max_sq + dC_max_sq)
    sapply(1:length(node_reports[, SERVICE_INDEX]),
	function(j) {
	    d = report_dist(node_reports, c_target, s_target, eta, dS_max_sq,
				dC_max_sq, S_MAX, C_MAX, j)
	    if(d >= t) {
		d = RESTRICTED_REPORT
	    }
	    d
	}
    )
}

find_s <- function(note_j) {
    (1 / 2) * (note_j**2 - note_j)
}

# Give the reports a weight based on how recent they were
weigh_reports <- function(lambda, theta, node_reports, report_distances, time) {
    sapply(1:length(node_reports[, SERVICE_INDEX]),
	function(j) {
	    theta_exp = ((find_s(node_reports[j, NOTE_INDEX]) + 1) *
			 (time - node_reports[j, TIME_INDEX]))
	    (lambda ** report_distances[j]) * (theta ** theta_exp)
	}
    )
}

# Find the trust values of the proxies
compute_trust <- function(network, R, w) {
    total_nodes = nrow(w)
    data.frame(
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
    )
}

# Select suitable entities for a target service
entity_selection <- function(network, lambda, theta, eta,
			     R, c_target, s_target, time) {
    total_nodes = length(R[, 1, 1])
    distances = sapply(1:total_nodes,
	function(i) {
	    restrict_reports(R[i, ,], c_target, s_target, eta)
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
    trusted_ids = T[order(-T$trust),]$id
    trusted_ids = trusted_ids[!trusted_ids %in% network$ill_reputed_nodes]
}

# Give a value stating the significance of older occurances
find_c_i <- function(theta, t_1, t_i) {
    theta ** (t_1 - t_i)
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
    network$reputation[[server]] = calculate_reputation(network, server, theta)
    if(network$reputation[[server]] < REPUTATION_THRESHOLD) {
    	network$ill_reputed_nodes[[length(network$ill_reputed_nodes) + 1]] =
    	    server
    }
    network
}

# Calculate the reputation of a server
calculate_reputation <- function(network, server, theta) {
    sum = 0
    for(j in seq(2, length(network$client_notes[[server]]))) {
    	client = network$clients[[server]][[j]]
	sum = sum +
	    find_c_i(theta, network$time_QR[[client]][1],
	    	     network$time_QR[[client]][1]) *
	    network$client_notes[[server]][[j]] *
	    head(network$QR[[client]], 1)
    }
    sum
}

# Simulate a transaction used at the initialization phase,
# add a report entry based on that
transaction <- function(server_service, server_capability,
			capability_target, service_target, R_QR,
                        time, client_is_malicious, client_attack_type,
                        client_rec_count) {
    report = rep(0, 4)
    report[SERVICE_INDEX] = service_target
    report[CAPABILITY_INDEX] = server_capability
    if(client_is_malicious) {
	if(client_attack_type == "bad mouther") {
	    report[NOTE_INDEX] = bad_mouth()
	} else if(client_attack_type == "good mouther") {
	    report[NOTE_INDEX] = good_mouth()
	} else {
	    report[NOTE_INDEX] = on_off(
		(floor(client_rec_count / 30) %% 2) == 1
	    )
	}
    } else {
	note = take_note(server_service, server_capability,
			 service_target, capability_target)
	report[NOTE_INDEX] = `if`(
	    runif(1) < R_QR,
	    note,
	    wrong_note(note)
	)
    }
    report[TIME_INDEX] = time
    return(report)
}

# Return the note value based on how a proxy will perform on a transaction
take_note <- function(server_service, server_capability, service_target, capability_target) {
    if(server_service < service_target) {
	-1
    } else if(server_service > service_target &&
		server_capability >= capability_target) {
	1
    } else {
	0
    }
}

# Return a note other than the one specified
wrong_note <- function(note) {
    wrong_vals = setdiff(c(-1, 0, 1), note)
    `if`(runif(1) < 0.5, wrong_vals[1], wrong_vals[2])
}

# Perform a transaction and update the values stored in the Trust Manager
transaction_and_update <- function(network, R, time, lambda, theta, eta,
                                   client, server, c_target, s_target) {
    R[server, client,] = transaction(
        network$service[server],
        network$capability[server],
	c_target,
	s_target,
	network$R_QR[[client]],
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
		    restrict_reports(R[i, ,], c_target, s_target, eta))
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
    list(R, network)
}

# Run some post initialization operations
post_init <- function(network, lambda, theta, eta, R, time, total_nodes, cs_targets) {
    server = entity_selection(network, lambda, theta, eta, R,
                              C_MAX - 1, cs_targets[[2]], time)[1]
    client = server
    while(client == server || client %in% network$ill_reputed_nodes) {
    	client = floor(runif(1, min=1, max=total_nodes))
    }
    result = transaction_and_update(network, R, time,
                                    lambda, theta, eta,
                                    client, server,
                                    network$capability[[server]],
                                    cs_targets[[2]])
    R = result[[1]]
    network = result[[2]]
    list(R, network)
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
    for(i in 1:phases) {
    	cat(sprintf("Transaction: %d\n", i))
    	cs_targets = floor(runif(2, min=1, max=S_MAX))
	R = initialize(network, R, time, lambda, theta, eta, cs_targets)
	if((i %% 100) == 0) {
	    time = time + 1
	}
	result = post_init(network, lambda, theta, eta, R,
			   time, total_nodes, cs_targets)
	R = result[[1]]
	network = result[[2]]
    }
    print("Ill Reputed Nodes")
    print(network$ill_reputed_nodes)
    graph_node_data(total_nodes, network, folder)
}
