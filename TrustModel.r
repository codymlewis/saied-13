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
    unlist(lapply(1:length(node_reports$service),
	function(j) {
	    d = report_dist(node_reports, s_target, c_target, eta, dS_max_sq,
				dC_max_sq, S_max, C_max, j)
	    if(is.nan(d) || d >= t) {
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
		(lambda ** report_distances[j]) *
		(theta ** ((find_s(node_reports$note[j]) + 1) *
            	(time - node_reports$time[j])))
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
			as.numeric(w[[i]][[j]]) *
			network$QR[[R[[i]]$sender[j]]][[1]] *
			R[[i]]$note[j]
		    )
		    denominator = denominator + as.numeric(w[[i]][[j]])
		}
		ifelse(denominator == 0, 0, numerator / denominator)
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
                weigh_reports(lambda, theta, R[[i]], d[[i]], time))
	}
    )
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

find_c_i <- function(theta, t_1, t_i) {
    theta ** (t_1 - t_i)
}

# Update the quality of recommendation of nodes that made reports on the server
# simultaneously calculates the reputation of the server
update_qrs <- function(network, R, w, client, server, client_note, theta, time) {
    network$reputation[[server]] = sum(unlist(lapply(1:length(R[[server]]$sender),
	function (j) {
	    X = R[[server]]$sender[j]
	    C_F = w[[X]] * network$QR[[client]][[1]]
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
		ifelse(denominator == 0,
		    0,
		    numerator / denominator),
		network$QR[[X]]
	    )
	    network$time_QR[[X]] <<- c(time, network$time_QR[[X]])
	    c_j = find_c_i(theta, network$time_QR[[X]][1], network$time_QR[[X]][j])
	    (c_j * R[[server]]$note[j] * network$QR[[X]][[1]])
	}
    )))
    network
}

# Simulate a transaction used at the inititilization phase, add a report entry based on that
transaction <- function(network, service_target, capability_target, client, server, reports, time) {
    j = length(reports$service) + 1
    reports$service[j] = service_target # * network$R_QR[client]
    reports$capability[j] = capability_target # * network$R_QR[client]
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
	client = server = 0
	while(client == server) {
	    client = floor(runif(1, min=1, max=length(network$service) + 1))
	    server = floor(runif(1, min=1, max=length(network$service) + 1))
	}
	cs_targets = floor(runif(2, min=1, max=101))
	result = transaction_and_update(network, R, time, lambda, theta, eta, client, server, cs_targets[[1]], cs_targets[[2]])
	R = result[[1]]
	network = result[[2]]
	time = result[[3]]
    }
    list(R, network)
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
		ifelse(is.null(r$service),
		    RESTRICTED_REPORT,
		    restrict_reports(r, s_target, c_target, eta))
	}
    )
    w = lapply(1:length(d),
	function(i) {
	    ifelse(d[[i]] == RESTRICTED_REPORT,
    		0, # if 0 then the corresponding values do nothing
    		weigh_reports(lambda, theta, R[[i]], d[[i]], time))
	}
    )
    network = update_qrs(network, R, w, client, server, result[[2]], theta, time)
    list(R, network, time)
}

# Run some post transaction operations
system_runtime <- function(network, lambda, theta, eta, R, time, total_nodes) {
    cs_targets = floor(runif(2, min=1, max=101))
    server = entity_selection(network, lambda, theta, eta, R, cs_targets[[1]], cs_targets[[2]], time)[1]
    client = server
    while(client == server) {
    	client = floor(runif(1, min=1, max=total_nodes))
    }
    result = transaction_and_update(network, R, time, lambda, theta, eta, client, server, cs_targets[[1]], cs_targets[[2]])
    print(sprintf("Client %d used Server %d", client, server))
    R = result[[1]]
    network = result[[2]]
    time = result[[3]]
    list(R, network, time)
}

# State how to use the program
help <- function() {
    paste(
    	"Run with arguments:",
	"--theta | -t <theta>\t\t\tValue of theta, indicates memory of the system",
	"--lambda | -l <lambda>\t\t\tValue of lambda, indicates memory of the system",
	"--eta | -e <eta>\t\t\tValue of eta, determines the amount of retained reports",
	"--total_nodes | -tn <total_nodes>\tThe number of nodes in the system",
	"--malicious | -m <malicious>\t\tPercentage of malicious nodes in decimal form",
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
    bootstrap_time = total_nodes * 10
    result = initialize(network, bootstrap_time, R, time, lambda, theta, eta)
    R = result[[1]]
    network = result[[2]]
    time = time + bootstrap_time
    print("A post bootstrap request")
    result = system_runtime(network, lambda, theta, eta, R, time, total_nodes)
    R = result[[1]]
    network = result[[2]]
    time = result[[3]]
    print("Reports")
    print(R)
    print("Network")
    print(network)
    cat(sprintf("Node: %d\nReal QR: %f\n", 1, network$R_QR[[1]]))
    print(rev(network$QR[[1]]))
    png(file = "Node_1_line.png")
    plot(
	rev(network$QR[[1]]),
	type="l",
	xlab="Number of interactions",
	ylab="Quality of Recommendation",
	main="Node 1 Quality of Recommendation"
    )
    dev.off()
    return(0)
}

main()
