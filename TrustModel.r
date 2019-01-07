#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2018-12-28
# Description:
# A simulation of the trust model described in
# http://people.cs.vt.edu/~irchen/5984/pdf/Saied-CS14.pdf

RESTRICTED_REPORT <- -1 # Marker showing that the report is restricted

# Calculate a 1 dimensional distance between 2 points in a vector
find_dist <- function(vec, target, current_index) {
    abs(target - vec[current_index])
}

# Find the distance between a report's context and a target context
report_dist <- function(node_reports, s_target, c_target, eta,
			dS_max_sq, dC_max_sq, S_max, C_max, j) {
    min(
        sqrt(
            (dS_max_sq + dC_max_sq) *
            ((find_dist(node_reports$service, s_target, j)**2 / dS_max_sq) +
            (find_dist(node_reports$capability, c_target, j)**2 / dC_max_sq))
        ),
        ifelse(
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
    )
}

# Find the distance of between a nodes reports and the target conditions
restrict_reports <- function(node_reports, s_target, c_target, eta) {
    dS_max_sq = find_dist(node_reports$service, s_target, length(node_reports$service))**2
    dC_max_sq = find_dist(node_reports$capability, c_target, length(node_reports$capability))**2
    S_max = node_reports$service[length(node_reports$service)]
    C_max = node_reports$capability[length(node_reports$capability)]
    t = sqrt(dS_max_sq + dC_max_sq)
    d <- c()
    for(j in seq(1, length(node_reports$service))) {
        d[j] = report_dist(node_reports, s_target, c_target, eta, dS_max_sq,
			    dC_max_sq, S_max, C_max, j)
        if(is.nan(d[j]) || d[j] >= t) {
            d[j] = RESTRICTED_REPORT
        }
    }
    d
}

find_s <- function(note_j) {
    (1 / 2) * (note_j**2 - note_j)
}

# Give the reports a weight based on how recent they were
weigh_reports <- function(lambda, theta, node_reports, report_distances, time) {
    w = c()
    for(j in seq(1, length(node_reports$service))) {
        w[j] = (lambda ** report_distances[j]) *
		(theta ** ((find_s(node_reports$note[j]) + 1) *
            	(time - node_reports$time[j])))
    }
    w
}

# Find the trust values of the proxies
compute_trust <- function(network, R, w) {
    trust_vals = c()
    for(i in seq(1, length(w))) {
        numerator = 0
        denominator = 0
        for(j in seq(1, length(w[[i]]))) {
	    numerator = numerator + (
		as.numeric(w[[i]][[j]]) *
		network$QR[[R[[i]]$sender[j]]][[1]] *
		R[[i]]$note[j]
	    )
	    denominator = denominator + as.numeric(w[[i]][[j]])
        }
        trust_vals[i] = ifelse(denominator == 0,
	    0,
	    numerator / denominator
	)
    }
    data.frame(
	id = seq(1, length(w)),
	trust = trust_vals
    )
}

# Select suitable entities for a target service
entity_selection <- function(network, lambda, theta, eta, R, s_target, c_target, time) {
    d = list()
    w = list()
    for(i in seq(1, length(R))) {
        d[[i]] = restrict_reports(R[[i]], s_target, c_target, eta)
        w[[i]] = ifelse(d[[i]] == RESTRICTED_REPORT,
                        0, # if 0 then the corresponding values do nothing
                        weigh_reports(lambda, theta, R[[i]], d[[i]], time))
    }
    T = compute_trust(network, R, w)
    T[order(-T$trust),]$id
}

# Return the note value based on how a proxy will perform on a transaction
take_note <- function(network, service_target, capability_target, proxy_id) {
    if(network$malicious[proxy_id]) {
	-1
    } else if(network$service[proxy_id] < service_target ||
		network$capability[proxy_id] < capability_target) {
	0
    } else {
	1
    }
}

# Update the quality of recommendation of nodes that made reports on the server
# simultaneously calculates the reputation of the server
update_qrs <- function(network, R, w, client, server, client_note, theta, time) {
    reputation = 0
    for(j in seq(1, length(R[[server]]$sender))) {
	X = R[[server]]$sender[j]
	C_F = w[[X]] * network$QR[[client]][[1]]
	QRXF = C_F * (-abs(R[[server]]$note[j] - client_note))
	numerator=denominator=0
	for(i in seq(1, length(network$QR[[X]]))) {
	    c_i = theta ** (network$time_QR[[X]][1] - network$time_QR[[X]][i])
	    numerator = numerator + (c_i * network$QR[[X]][[i]] + QRXF)
	    denominator = denominator + (c_i + abs(C_F))
	}
	network$QR[[X]] = c(
	    ifelse(denominator == 0,
		0,
		numerator / denominator),
	    network$QR[[X]]
	)
	network$time_QR[[X]] = c(time, network$time_QR[[X]])
	reputation = reputation + (c_i * R[[server]]$note[j] * network$QR[[X]][[1]])
    }
    network$reputation[[server]] = reputation
    network
}

# Simulate a transaction used at the inititilization phase, add a report entry based on that
transaction <- function(network, service_target, capability_target, client, server, reports, time) {
    j = length(reports$service) + 1
    reports$service[j] = service_target * network$R_QR[client]
    reports$capability[j] = capability_target * network$R_QR[client]
    reports$note[j] = ifelse(
    	runif(1) < network$R_QR[client],
    	take_note(network, service_target, capability_target, server),
	-take_note(network, service_target, capability_target, server)
    )
    reports$time[j] = time
    reports$sender[j] = client
    list(reports, reports$note[j])
}

# Develop a collection of reports on the network
initialize <- function(network, bootstrap_time, R, time, lambda, theta, eta) {
    for(i in seq(1, bootstrap_time)) {
	time = time + 1
	client = server = 0
	while(client == server) {
	    client = floor(runif(1, min=1, max=length(network$service) + 1))
	    server = floor(runif(1, min=1, max=length(network$service) + 1))
	}
	s_target = floor(runif(1, min=1, max=101))
	c_target = floor(runif(1, min=1, max=101))

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
	d = list()
	w = list()
	for(i in seq(1, length(R))) {
	    d[[i]] = ifelse(is.null(R[[i]]$service),
	    		    RESTRICTED_REPORT,
	    		    restrict_reports(R[[i]], s_target, c_target, eta))
	    w[[i]] = ifelse(d[[i]] == RESTRICTED_REPORT,
			    0, # if 0 then the corresponding values do nothing
			    weigh_reports(lambda, theta, R[[i]], d[[i]], time))
	}
	network = update_qrs(network, R, w, client, server, result[[2]], theta, time)
    }
    list(R, network)
}

# State how to use the program
help <- function() {
    paste(
    	"Run with arguments:",
	"--theta | -t <theta>		    Value of theta, indicates memory of the system",
	"--lambda | -l <lambda>		    Value of lambda, indicates memory of the system",
	"--eta | -e <eta>		    Value of eta, determines the amount of retained reports",
	"--total_nodes | -t <total_nodes>    The number of nodes in the system",
	"--malicious | -m <malicious>	    Percentage of malicious nodes in decimal form",
	sep = "\n"
    )
}

main <- function() {
    args = commandArgs(trailingOnly=TRUE)
    theta=lambda=eta=total_nodes=malicious_percent=0
    if(length(args) == 0) {
    	cat(help(), "\n")
    	return(0)
    }
    for(i in seq(1, length(args), by=2)) {
        if(args[i] == "--theta" || args[i] == "-t") {
            theta = as.numeric(args[i + 1])
        } else if(args[i] == "--lambda" || args[i] == "-l") {
            lambda = as.numeric(args[i + 1])
        } else if(args[i] == "--eta" || args[i] == "-e") {
            eta = as.numeric(args[i + 1])
	} else if(args[i] == "--total-nodes" || args[i] == "-tn") {
	    total_nodes = as.numeric(args[i + 1])
	} else if(args[i] == "--malicious" || args[i] == "-m") {
	    malicious_percent = as.numeric(args[i + 1])
	}
    }
    print(sprintf("theta : %f, lambda : %f, eta : %d", theta, lambda, eta))
    time = 0
    network = list(
	id = seq(1, total_nodes),
	energy = rep(100, each=total_nodes),
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
    for(i in seq(1, total_nodes)) {
	R[[i]] = list(
	    service = c(),
	    capability = c(),
	    note = c(),
	    time = c(),
	    sender = c()
	)
    }
    bootstrap_time = total_nodes * 10
    result = initialize(network, bootstrap_time, R, time, lambda, theta, eta)
    R = result[[1]]
    network = result[[2]]
    time = time + bootstrap_time
    print("Reports")
    print(R)
    print("network")
    print(network)
    print("Most trusted nodes")
    print(entity_selection(network, lambda, theta, eta, R, 50, 50, time))
    server = entity_selection(network, lambda, theta, eta, R, 50, 50, time)[1]
    client = server
    while(client == server) {
    	client = floor(runif(1, min=1, max=total_nodes))
    }
    # transaction()
    return(0)
}

main()
