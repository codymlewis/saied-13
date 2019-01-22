#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2018-12-28
# Description:
# A simulation of the trust model described in
# http://people.cs.vt.edu/~irchen/5984/pdf/Saied-CS14.pdf

source("Attacks.r")
source("TrustManager.r")

RESTRICTED_REPORT <- -1 # Marker showing that the report is restricted
EPSILON <- 5 # A small value used on determining which note to give
REPUTATION_THRESHOLD <- -1 # Point where a node is so ill reputed that it is
                           # no longer interacted with, in the network
S_MAX = 101
C_MAX = 101

# Develop a collection of reports on the network
initialize <- function(network, R, time, lambda, theta, eta) {
    for(i in seq(1, length(network$service))) {
	if(!i %in% network$ill_reputed_nodes) {
	    for(j in seq(1, length(network$service))) {
	    	if(!j %in% network$ill_reputed_nodes) {
		    cs_targets = floor(runif(2, min=1, max=101))
		    R[i, ,] = transaction(
			network,
			cs_targets[[1]],
			cs_targets[[2]],
			j,
			i,
			R[i, , ],
			time
		    )[[1]]
		}
	    }
	}
    }
    R
}

# Calculate a 1 dimensional distance between 2 points in a vector
find_dist <- function(target, current) {
    abs(target - current)
}

# Find the distance between a report's context and a target context
report_dist <- function(node_reports, s_target, c_target, eta,
			dS_max_sq, dC_max_sq, S_max, C_max, j) {
    shared_term = sqrt(
	(dS_max_sq + dC_max_sq) *
	((find_dist(s_target, node_reports[j, SERVICE_INDEX])**2 / dS_max_sq) +
	(find_dist(c_target, node_reports[j, CAPABILITY_INDEX])**2 / dC_max_sq))
    )
    unique_term = `if`(
	node_reports[j, NOTE_INDEX] >= 0,
	sqrt(
	    (dS_max_sq + dC_max_sq) *
	    (((S_max - node_reports[j, SERVICE_INDEX]) / (S_max - (s_target - eta)))**2 +
	    (node_reports[j, CAPABILITY_INDEX] / (c_target + eta))**2)
	),
	sqrt(
	    (dS_max_sq + dC_max_sq) *
	    (((C_max - node_reports[j, CAPABILITY_INDEX]) / (C_max - (c_target - eta)))**2 +
	    (node_reports[j, SERVICE_INDEX] / (s_target + eta))**2)
	)
    )
    min(shared_term, unique_term)
}

# Find the distance of between a nodes reports and the target conditions
restrict_reports <- function(node_reports, s_target, c_target, eta) {
    dS_max_sq = find_dist(s_target, S_MAX)**2
    dC_max_sq = find_dist(c_target, C_MAX)**2
    t = sqrt(dS_max_sq + dC_max_sq)
    unlist(lapply(1:length(node_reports[, SERVICE_INDEX]),
	function(j) {
	    d = report_dist(node_reports, s_target, c_target, eta, dS_max_sq,
				dC_max_sq, S_MAX, C_MAX, j)
	    if(d >= t) {
		d = RESTRICTED_REPORT
	    }
	    d
	}
    ))
}

find_s <- function(note_j) {
    (1 / 2) * (note_j**2 - note_j)
}

# Give the reports a weight based on how recent they were
weigh_reports <- function(lambda, theta, node_reports, report_distances, time) {
    unlist(lapply(1:length(node_reports[, SERVICE_INDEX]),
	function(j) {
	    theta_exp = ((find_s(node_reports[j, NOTE_INDEX]) + 1) *
			 (time - node_reports[j, TIME_INDEX]))
	    (lambda ** report_distances[j]) * (theta ** theta_exp)
	}
    ))
}

# Find the trust values of the proxies
compute_trust <- function(network, R, w) {
    data.frame(
	id = seq(1, length(w)),
	trust = unlist(lapply(1:length(w),
	    function(i) {
		numerator = 0
		denominator = 0
		for(j in seq(1, length(w[[i]]))) {
		    numerator = numerator + (
			w[[i]][[j]] *
			network$QR[[j]][[1]] *
			R[i, j, NOTE_INDEX]
		    )
		    denominator = denominator + w[[i]][[j]]
		}
		`if`(denominator == 0, 0, numerator / denominator)
	    }
	))
    )
}

# Select suitable entities for a target service
entity_selection <- function(network, lambda, theta, eta, R, c_target, s_target, time) {
    d = lapply(1:length(R[, 1, 1]),
	function(i) {
	    restrict_reports(R[i, ,], s_target, c_target, eta)
	}
    )
    w = lapply(1:length(R[, 1, 1]),
	function(i) {
	    ifelse(d[[i]] == RESTRICTED_REPORT,
                0, # if 0 then the corresponding values do nothing
                weigh_reports(lambda, theta, R[i, ,], d[[i]], time)
	    )
	}
    )
    T = compute_trust(network, R, w)
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
    	if(w[[server]][[j]] != 0) {
	    X = j # To make the equations look like those on the paper
	    C_F = w[[server]][[X]] * network$QR[[client]][[1]]
	    QRXF = C_F * (-abs(R[server, X, NOTE_INDEX] - client_note))
	    numerator=denominator=0
	    numerator = sum(unlist(lapply(1:length(network$QR[[X]]),
		function(i) {
		    c_i = find_c_i(theta, network$time_QR[[X]][1], network$time_QR[[X]][i])
		    c_i * network$QR[[X]][[i]] + QRXF
		}
	    )))
	    denominator = sum(unlist(lapply(1:length(network$QR[[X]]),
		function(i) {
		    c_i = find_c_i(theta, network$time_QR[[X]][1], network$time_QR[[X]][i])
		    c_i + abs(C_F)
		}
	    )))
	    network$QR[[X]] = c(
		`if`(denominator == 0,
		    0,
		    numerator / denominator),
		network$QR[[X]]
	    )
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
transaction <- function(network, capability_target, service_target,
                        client, server, reports, time) {
    j = client
    reports[j, SERVICE_INDEX] = service_target
    reports[j, CAPABILITY_INDEX] = network$capability[[server]]
    if(network$malicious[[client]]) {
	if(network$attack_type[[client]] == "bad mouther") {
	    reports[j, NOTE_INDEX] = bad_mouth(
		network$service[[server]], network$capability[[server]],
		service_target, capability_target
	    )
	} else if(network$attack_type[[client]] == "good mouther") {
	    reports[j, NOTE_INDEX] = good_mouth(
		network$service[[server]], network$capability[[server]],
		service_target, capability_target
	    )
	} else {
	    reports[j, NOTE_INDEX] = on_off(
	    	network$is_bad_mouthing[[client]],
		network$service[[server]], network$capability[[server]],
		service_target, capability_target
	    )
	    network$toggle_count[[client]] =
	        (network$toggle_count[[client]] + 1) %% ON_OFF_TOGGLE
	    if(network$toggle_count[[client]] == 0) {
		network$is_bad_mouthing[[client]] =
		    !network$is_bad_mouthing[[client]]
	    }
	}
    } else {
	note = take_note(network$service[[server]], network$capability[[server]],
			 service_target, capability_target)
	reports[j, NOTE_INDEX] = `if`(
	    runif(1) < network$R_QR[client],
	    note,
	    wrong_note(note)
	)
    }
    reports[j, TIME_INDEX] = time
    list(reports, reports[j, NOTE_INDEX])
}

# Return the note value based on how a proxy will perform on a transaction
take_note <- function(server_service, server_capability, service_target, capability_target) {
    if(server_service < service_target ||
	server_capability < capability_target) {
	-1
    } else if(server_service > service_target + EPSILON &&
		server_capability > capability_target + EPSILON) {
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
    time = time + 1
    result = transaction(
	network,
	c_target,
	s_target,
	client,
	server,
	R[server, ,],
	time
    )
    R[server, ,] = result[[1]]
    d = lapply(1:length(R[, 1, 1]),
    	function(i) {
		`if`(is.null(R[i, , SERVICE_INDEX]),
		    RESTRICTED_REPORT,
		    restrict_reports(R[i, ,], s_target, c_target, eta))
	}
    )
    w = lapply(1:length(d),
	function(i) {
	    ifelse(d[[i]] == RESTRICTED_REPORT,
    		0, # if 0 then the corresponding values do nothing
    		weigh_reports(lambda, theta, R[i, ,], d[[i]], time)
    	    )
	}
    )
    network = update_qrs(network, R, w, client, server,
                         result[[2]], theta, time)
    list(R, network, time)
}

# Run some post initialization operations
post_init <- function(network, lambda, theta, eta, R, time, total_nodes, cs_targets) {
    # cs_targets = floor(runif(2, min=1, max=101))
    server = entity_selection(network, lambda, theta, eta, R,
                              cs_targets[[1]], cs_targets[[2]], time)[1]
    client = server
    while(client == server || client %in% network$ill_reputed_nodes) {
    	client = floor(runif(1, min=1, max=total_nodes))
    }
    result = transaction_and_update(network, R, time,
                                    lambda, theta, eta,
                                    client, server,
                                    cs_targets[[1]], cs_targets[[2]])
    if(network$constrained[[server]] && network$capability[[server]] > 1) {
    	network$capability[[server]] = network$capability[[server]] - 1
    }
    R = result[[1]]
    network = result[[2]]
    time = result[[3]]
    list(R, network, time)
}

# Run through the system operations
run <- function(lambda, theta, eta, total_nodes, malicious_percent,
		phases, folder, attack_type, poor_witnesses, constrained) {
    time = 0
    network = create_network(total_nodes, malicious_percent, time,
			     S_MAX, C_MAX, poor_witnesses, constrained)
    network = assign_attack_types(network, malicious_percent,
    				  total_nodes, attack_type)
    R = create_report_set(total_nodes)
    for(i in seq(1, phases)) {
    	print(sprintf("Transaction: %d", i))
	R = initialize(network, R, time, lambda, theta, eta)
	time = time + 1
    	cs_targets = runif(2, min=1, max=S_MAX)
	result = post_init(network, lambda, theta, eta, R, time, total_nodes, cs_targets)
	R = result[[1]]
	network = result[[2]]
	time = result[[3]]
    }
    print("Ill Reputed Nodes")
    print(network$ill_reputed_nodes)
    graph_node_data(total_nodes, network, folder)
}
