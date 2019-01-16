#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2019-01-16
# Description:
# Creates data structures that the trust manager manages

# Create the IoT network
create_network <- function(total_nodes, malicious_percent, time) {
    list(
		# Basic node data
		id = seq(1, total_nodes),
		service = floor(runif(total_nodes, min=1, max=101)),
		capability = floor(runif(total_nodes, min=1, max=101)),
		R_QR = runif(total_nodes),
		QR = rep(list(1), each=total_nodes),
		time_QR = rep(list(time), each=total_nodes),
		# Attack based things
		malicious = c(rep(FALSE, each=ceiling(total_nodes * (1 - malicious_percent))),
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
    R = list()
    R = lapply(1:total_nodes, function(i) {
	R[[i]] = list(
	    service = c(),
	    capability = c(),
	    note = c(),
	    time = c()
	)
    })
    R
}
