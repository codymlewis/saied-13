#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2018-12-28
# Description:
# A simulation of the trust model described in
# http://people.cs.vt.edu/~irchen/5984/pdf/Saied-CS14.pdf

source("Attacks.r")

RESTRICTED_REPORT <- -1 # Marker showing that the report is restricted
EPSILON <- 5 # A small value used on determining which note to give

# Develop a collection of reports on the network
initialize <- function(network, bootstrap_time, R, time, lambda, theta, eta) {
    for(i in seq(1, bootstrap_time)) {
    	time = time + 1
	client = server = 0
	while(client == server) {
	    client = floor(runif(1, min=1, max=length(network$service) + 1))
	    server = floor(runif(1, min=1, max=length(network$service) + 1))
	}
	cs_targets = floor(runif(2, min=1, max=101))
	R[[server]] = transaction(
	    network,
	    cs_targets[[1]],
	    cs_targets[[2]],
	    client,
	    server,
	    R[[server]],
	    time
	)[[1]]
    }
    R
}

# Calculate a 1 dimensional distance between 2 points in a vector
find_dist <- function(vec, target, current) {
    abs(target - current)
}

# Find the distance between a report's context and a target context
report_dist <- function(node_reports, s_target, c_target, eta,
			dS_max_sq, dC_max_sq, S_max, C_max, j) {
    shared_term = sqrt(
	(dS_max_sq + dC_max_sq) *
	((find_dist(node_reports$service, s_target, j)**2 / dS_max_sq) +
	(find_dist(node_reports$capability, c_target, j)**2 / dC_max_sq))
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
    dS_max_sq = find_dist(node_reports$service, s_target, S_max)**2
    dC_max_sq = find_dist(node_reports$capability, c_target, S_max)**2
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
			network$QR[[R[[i]]$sender[j]]][[1]] *
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
entity_selection <- function(network, lambda, theta, eta, R, s_target, c_target, time) {
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
    T[order(-T$trust),]$id
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
    network$reputation[[server]] = sum(unlist(lapply(1:length(R[[server]]$sender),
	function (j) {
	    X = R[[server]]$sender[j]
	    # print(sprintf("Weight length: %d, R[[server]]$sender length: %d", length(w[[X]]), length(R[[server]]$sender)))
	    # print(sprintf("w[[%d]]", X))
	    # print(w[[X]])
	    # print("Time of QRs")
	    # print(network$time_QR[[X]])
	    C_F = tail(w[[X]], 1) * network$QR[[client]][[1]]
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
	    # print(sprintf("X: %d, C_F: %f, QRXF: %e, numerator: %f, denominator: %f", X, C_F, QRXF, numerator, denominator))
	    network$QR[[X]] <<- c(
		`if`(denominator == 0,
		    0,
		    numerator / denominator),
		network$QR[[X]]
	    )
	    network$time_QR[[X]] <<- c(time, network$time_QR[[X]])
	    c_j = find_c_i(theta, network$time_QR[[X]][1], network$time_QR[[X]][1])
	    # print(sprintf("c_%d: %f", j, c_j))
	    (c_j * R[[server]]$note[j] * network$QR[[X]][[1]])
	}
    )))
    network
}

# Return a note other than the one specified
wrong_note <- function(note) {
    wrong_vals = setdiff(c(-1, 0, 1), note)
    `if`(runif(1) < 0.5, wrong_vals[1], wrong_vals[2])
}

# Simulate a transaction used at the initialization phase, add a report entry based on that
transaction <- function(network, service_target, capability_target, client, server, reports, time) {
    j = length(reports$service) + 1
    reports$service[j] = service_target # * network$R_QR[client]
    reports$capability[j] = capability_target # * network$R_QR[client]
    if(network$malicious[client]) {
    	reports$note[j] = bad_mouth()
    } else {
	note = take_note(network, service_target, capability_target, server)
	reports$note[j] = `if`(
	    runif(1) < network$R_QR[client],
	    note,
	    wrong_note(note)
	)
    }
    reports$time[j] = time
    reports$sender[j] = client
    list(reports, reports$note[j])
}

# Perform a transaction and update the values stored in the Trust Manager
transaction_and_update <- function(network, R, time, lambda, theta, eta, client, server, c_target, s_target) {
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
    network = update_qrs(network, R, w, client, server, result[[2]], theta, time)
    list(R, network, time)
}

# Run some post initialization operations
post_init <- function(network, lambda, theta, eta, R, time, total_nodes) {
    cs_targets = floor(runif(2, min=1, max=101))
    server = entity_selection(network, lambda, theta, eta, R, cs_targets[[1]], cs_targets[[2]], time)[1]
    client = server
    while(client == server) {
    	client = floor(runif(1, min=1, max=total_nodes))
    }
    result = transaction_and_update(network, R, time, lambda, theta, eta, client, server, cs_targets[[1]], cs_targets[[2]])
    R = result[[1]]
    network = result[[2]]
    time = result[[3]]
    list(R, network, time)
}

# Run through the system operations
run <- function(lambda, theta, eta, total_nodes, malicious_percent, phases) {
    time = 0
    network = list(
	id = seq(1, total_nodes),
	service = floor(runif(total_nodes, min=1, max=101)),
	capability = floor(runif(total_nodes, min=1, max=101)),
	malicious = c(rep(FALSE, each=(total_nodes * (1 - malicious_percent))),
				  rep(TRUE, each=(total_nodes * malicious_percent))),
	R_QR = runif(total_nodes),
	QR = rep(list(1), each=total_nodes),
	time_QR = rep(list(time), each=total_nodes),
	reputation = rep(1, each=total_nodes)
    )
    R = list()
    R = lapply(1:total_nodes, function(i) {
	R[[i]] = list(
	    service = c(),
	    capability = c(),
	    note = c(),
	    time = c(),
	    sender = c()
	)
    })
    for(i in seq(1, phases)) {
    	print(sprintf("Phase run: %d", i))
	bootstrap_time = 50 * total_nodes
	R = initialize(network, bootstrap_time, R, time, lambda, theta, eta)
	time = time + bootstrap_time
	result = post_init(network, lambda, theta, eta, R, time, total_nodes)
	R = result[[1]]
	network = result[[2]]
	time = result[[3]]
    }
    graph_node_data(total_nodes, network)
}

# Create graphs on each of the nodes
graph_node_data <- function(total_nodes, network) {
    for(i in seq(1, total_nodes)) {
	cat(sprintf("Node: %4d\tQR: %f\tReal QR: %f\n", i, network$QR[[i]][[1]], network$R_QR[[i]]))
	png(file = sprintf("graphs/Node_%d_line.png", i))
	plot(
	    rev(network$QR[[i]]),
	    type="l",
	    xlab="Number of Recommendations",
	    ylab="Quality of Recommendation",
	    xlim=range(0, length(network$QR[[i]])),
	    ylim=range(-1.5, 1.5),
	    main=sprintf("Node %d Quality of Recommendation", i)
	)
	legend(0, 1.5, sprintf("R_QR: %f", network$R_QR[[i]]))
	legend(5 * length(network$QR[[i]]) / 8, 1.5, c(sprintf("Final QR: %f", head(network$QR[[i]], 1)), sprintf("Reputation: %f", network$reputation[[i]])))
	dev.off()
    }
}
