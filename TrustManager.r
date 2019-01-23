#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2019-01-16
# Description:
# Creates data structures that the trust manager manages

SERVICE_INDEX <- 1
CAPABILITY_INDEX <- 2
NOTE_INDEX <- 3
TIME_INDEX <- 4

# Create the IoT network
create_network <- function(total_nodes, malicious_percent, time,
                           S_max, C_max, poor_witnesses, constrained) {
    list(
	# Basic node data
	id = seq(1, total_nodes),
	service = floor(runif(total_nodes, min=1, max=S_MAX)),
	capability = floor(runif(total_nodes, min=1, max=C_MAX)),
	constrained = c(rep(TRUE, each=constrained * total_nodes),
	                rep(FALSE, each=(1 - constrained) * total_nodes)),
	R_QR = c(runif(poor_witnesses * total_nodes),
		 rep(1, each=(1 - poor_witnesses) * total_nodes)),
	QR = rep(list(1), each=total_nodes),
	time_QR = rep(list(time), each=total_nodes),
	# Attack based things
	malicious = c(rep(FALSE,
			  each=ceiling(total_nodes * (1 - malicious_percent))),
			    rep(TRUE, each=ceiling(total_nodes * malicious_percent))),
	attack_type = rep("f", each=total_nodes),
	toggle_count = rep(0, each=total_nodes), # For on-off attacks
	is_bad_mouthing = rep(TRUE, each=total_nodes),
	# Server based things
	client_notes = rep(list(0), each=total_nodes),
	clients = rep(list(0), each=total_nodes),
	# Trust manager data
	reputation = rep(1, each=total_nodes),
	ill_reputed_nodes = c()
    )
}

# Create the set of reports that will describe each of the nodes
create_report_set <- function(total_nodes) {
    fill_data = rep(0, total_nodes * total_nodes * 4)
    array(fill_data, c(total_nodes, total_nodes, 4))
}

create_qr_set <- function(total_nodes, phases) {
    fill_data = rep(0, total_nodes * phases * 4)
    array(fill_data, c(total_nodes, phases, 4))
}

# Create graphs on each of the nodes
graph_node_data <- function(total_nodes, network, folder) {
    dir.create(sprintf("./graphs/%s", folder), showWarnings=FALSE)
    for(i in seq(1, total_nodes)) {
	cat(sprintf("Node: %4d\tQR: %f\tReal QR: %f\tR_QR - QR: %f\n",
		i, network$QR[[i]][[1]], network$R_QR[[i]],
		network$R_QR[[i]] - network$QR[[i]][[1]]
	))
	png(file = sprintf("graphs/%s/Node_%d_line.png", folder, i))
	plot(
	    rev(network$QR[[i]]),
	    type="l",
	    xlab="Number of Recommendations",
	    ylab="Quality of Recommendation",
	    xlim=range(0, length(network$QR[[i]])),
	    ylim=range(-1.5, 1.5),
	    main=sprintf("Node %d Quality of Recommendation", i)
	)
	text(
	     length(network$QR[[i]]) / 2,
	     1.5,
	     sprintf("R_QR: %f\tFinal QR: %f\tReputation: %f",
		 network$R_QR[[i]],
		 head(network$QR[[i]], 1),
		 network$reputation[[i]]),
	     cex=0.8
	)
	text(
		 length(network$QR[[i]]) / 2,
		 -1.4,
		 sprintf("service: %d\tcapability: %d",
		     network$service[[i]],
		     network$capability[[i]]),
		 cex=0.8
	)
	if(network$malicious[[i]]) {
		text(
			length(network$QR[[i]]) / 2,
			-1.5,
			network$attack_type[[i]],
			cex=0.8
		)
	}
	dev.off()
    }
}
