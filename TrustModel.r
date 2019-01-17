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

# Develop a collection of reports on the network
initialize <- function(network, R, time, lambda, theta, eta,
		       c_target, s_target) {
    for(i in seq(1, length(network$service))) {
	if(!i %in% network$ill_reputed_nodes) {
	    for(j in seq(1, length(network$service))) {
	    	if(!j %in% network$ill_reputed_nodes) {
		    R[[i]] = transaction(
			network,
			s_target,
			c_target,
			j,
			i,
			R[[i]],
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
	((find_dist(s_target, node_reports$service[[j]])**2 / dS_max_sq) +
	(find_dist(c_target, node_reports$capability[[j]])**2 / dC_max_sq))
    )
    unique_term = `if`(
	node_reports$note[j] >= 0,
	sqrt(
	    (dS_max_sq + dC_max_sq) *
	    (((S_max - node_reports$service[j]) / (S_max - (s_target - eta)))**2 +
	    (node_reports$capability[j] / (c_target + eta))**2)
	),
	sqrt(
	    (dS_max_sq + dC_max_sq) *
	    (((C_max - node_reports$capability[j]) / (C_max - (c_target - eta)))**2 +
	    (node_reports$service[j] / (s_target + eta))**2)
	)
    )
    `if`(is.nan(shared_term) || is.nan(unique_term),
    	`if`(is.nan(shared_term), unique_term, shared_term),
    	min(shared_term, unique_term))
}

# Find the distance of between a nodes reports and the target conditions
restrict_reports <- function(node_reports, s_target, c_target, eta) {
    S_max = max(node_reports$service)
    C_max = max(node_reports$capability)
    dS_max_sq = find_dist(s_target, S_max)**2
    dC_max_sq = find_dist(c_target, C_max)**2
    t = sqrt(dS_max_sq + dC_max_sq)
    unlist(lapply(1:length(node_reports$service),
	function(j) {
	    d = report_dist(node_reports, s_target, c_target, eta, dS_max_sq,
				dC_max_sq, S_max, C_max, j)
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
    unlist(lapply(1:length(node_reports$service),
	function(j) {
	    theta_exp = ((find_s(node_reports$note[j]) + 1) *
			 (time - node_reports$time[j]))
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
			R[[i]]$note[j]
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
    d = lapply(R,
	function(r) {
	    restrict_reports(r, s_target, c_target, eta)
	}
    )
    w = lapply(1:length(R),
	function(i) {
	    ifelse(d[[i]] == RESTRICTED_REPORT,
                0, # if 0 then the corresponding values do nothing
                weigh_reports(lambda, theta, R[[i]], d[[i]], time)
	    )
	}
    )
    T = compute_trust(network, R, w)
    trusted_ids = T[order(-T$trust),]$id
    trusted_ids = trusted_ids[!trusted_ids %in% network$ill_reputed_nodes]
}

# Return the note value based on how a proxy will perform on a transaction
take_note <- function(network, service_target, capability_target, proxy_id) {
    if(network$service[proxy_id] < service_target ||
	network$capability[proxy_id] < capability_target) {
	-1
    } else if(network$service[proxy_id] > service_target + EPSILON &&
		network$capability[proxy_id] > capability_target + EPSILON) {
	1
    } else {
	0
    }
}

# Give a value stating the significance of older occurances
find_c_i <- function(theta, t_1, t_i) {
    theta ** (t_1 - t_i)
}

# Update the quality of recommendation of nodes that made reports on the server
# simultaneously calculates the reputation of the server
update_qrs <- function(network, R, w, client, server, client_note, theta, time) {
    lapply(1:length(R[[server]]$service),
	function (j) {
	    X = j # To make the equations look like those on the paper
	    C_F = w[[server]][[j]] * network$QR[[client]][[1]]
	    QRXF = C_F * (-abs(R[[server]]$note[j] - client_note))
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
	    network$QR[[X]] <<- c(
		`if`(denominator == 0,
		    0,
		    numerator / denominator),
		network$QR[[X]]
	    )
	    network$time_QR[[X]] <<- c(time, network$time_QR[[X]])
	}
    )
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
	    find_c_i(theta, network$time_QR[[client]][1], network$time_QR[[client]][1]) *
	    network$client_notes[[server]][[j]] *
	    head(network$QR[[client]], 1)
    }
    sum
}

# Return a note other than the one specified
wrong_note <- function(note) {
    wrong_vals = setdiff(c(-1, 0, 1), note)
    `if`(runif(1) < 0.5, wrong_vals[1], wrong_vals[2])
}

# Approximate t in order to propagate the effects of R_QR
approximate_t <- function(s_target, c_target) {
    s_max=c_max=100 # Approximate the max values as the maximum possible
    sqrt(abs(s_target - s_max)**2 + abs(c_target - c_max)**2)
}

# Factor to make an impact of R_QR to the context
find_factor <- function(t, network, client) {
    `if`(
    	runif(1) < 0.5,
    	-t * (1 - network$R_QR[[client]]),
    	t * (1 - network$R_QR[[client]])
    )
}

# Simulate a transaction used at the initialization phase,
# add a report entry based on that
transaction <- function(network, service_target, capability_target,
                        client, server, reports, time) {
    j = client
    t = approximate_t(service_target, capability_target)
    s_factor = find_factor(t, network, client)
    reports$service[j] = service_target + s_factor
    c_factor = find_factor(t, network, client)
    reports$capability[j] = capability_target + c_factor
    if(network$malicious[[client]]) {
	if(network$attack_type[[client]] == "bad mouther") {
	    reports$note[j] = bad_mouth()
	} else if(network$attack_type[[client]] == "good mouther") {
	    reports$note[j] = good_mouth()
	} else {
	    reports$note[j] = on_off(network$is_bad_mouthing[[client]])
	    network$toggle_count[[client]] =
	        (network$toggle_count[[client]] + 1) %% ON_OFF_TOGGLE
	    if(network$toggle_count[[client]] == 0) {
		network$is_bad_mouthing[[client]] =
		    !network$is_bad_mouthing[[client]]
	    }
	}
    } else {
	note = take_note(network, service_target, capability_target, server)
	reports$note[j] = `if`(
	    runif(1) < network$R_QR[client],
	    note,
	    wrong_note(note)
	)
    }
    reports$time[j] = time
    list(reports, reports$note[j])
}

# Perform a transaction and update the values stored in the Trust Manager
transaction_and_update <- function(network, R, time, lambda, theta, eta,
                                   client, server, c_target, s_target) {
    time = time + 1
    result = transaction(
	network,
	s_target,
	c_target,
	client,
	server,
	R[[server]],
	time
    )
    R[[server]] = result[[1]]
    d = lapply(R,
    	function(r) {
		`if`(is.null(r$service),
		    RESTRICTED_REPORT,
		    restrict_reports(r, s_target, c_target, eta))
	}
    )
    w = lapply(1:length(d),
	function(i) {
	    ifelse(d[[i]] == RESTRICTED_REPORT,
    		0, # if 0 then the corresponding values do nothing
    		weigh_reports(lambda, theta, R[[i]], d[[i]], time)
    	    )
	}
    )
    network = update_qrs(network, R, w, client, server,
                         result[[2]], theta, time)
    list(R, network, time)
}

# Run some post initialization operations
post_init <- function(network, lambda, theta, eta, R, time, total_nodes, cs_targets) {
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
    R = result[[1]]
    network = result[[2]]
    time = result[[3]]
    list(R, network, time)
}

# Run through the system operations
run <- function(lambda, theta, eta, total_nodes,
                malicious_percent, phases, folder) {
    time = 0
    network = create_network(total_nodes, malicious_percent, time)
    network = assign_attack_types(network, malicious_percent, total_nodes)
    R = create_report_set(total_nodes)
    for(i in seq(1, phases)) {
    	print(sprintf("Transaction: %d", i))
	cs_targets = floor(runif(2, min=1, max=101))
	R = initialize(network, R, time, lambda, theta, eta,
		       cs_targets[[1]], cs_targets[[2]])
	time = time + 1
	result = post_init(network, lambda, theta, eta, R, time, total_nodes, cs_targets)
	R = result[[1]]
	network = result[[2]]
	time = result[[3]]
    }
    print("Ill Reputed Nodes")
    print(network$ill_reputed_nodes)
    graph_node_data(total_nodes, network, folder)
}
