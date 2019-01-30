#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2019-01-16
# Description:
# Creates data structures that the trust manager manages

SERVICE_INDEX <- 1
CAPABILITY_INDEX <- 2
NOTE_INDEX <- 3
TIME_INDEX <- 4

SERVICES <- c(16, 33, 50, 66, 83, 100)

# Get the service requirements of a random service
get_random_service <- function() {
    return(SERVICES[[floor(runif(1, 1, length(SERVICES) + 1))]])
}

# Create the IoT network
create_network <- function(total_nodes, malicious_percent, time,
                           S_max, C_max, poor_witnesses, constrained) {
    return(
        list(
            # Basic node data
            id = seq(1, total_nodes),
            service = c(floor(runif(constrained * total_nodes, min=1, max=S_MAX)),
                        rep(100, each=(1 - constrained) * total_nodes)),
            capability = c(floor(runif(constrained * total_nodes, min=1, max=C_MAX)),
                            rep(100, each=(1 - constrained) * total_nodes)),
            accurate_note_take = c(runif(poor_witnesses * total_nodes),
                                    rep(1, each=(1 - poor_witnesses) * total_nodes)),
            QR = rep(list(1), each=total_nodes),
            time_QR = rep(list(time), each=total_nodes),
            # Attack based things
            malicious = c(
                          rep(FALSE,
                              each=ceiling(total_nodes * (1 - malicious_percent))),
                          rep(TRUE, each=ceiling(total_nodes * malicious_percent))
            ),
            attack_type = rep("f", each=total_nodes),
            recommendations_count = rep(0, each=total_nodes), # For on-off
            # Reputation calculation based variables
            client_notes = rep(list(0), each=total_nodes),
            clients = rep(list(0), each=total_nodes),
            client_QRs = rep(list(0), each=total_nodes),
            client_time_QRs = rep(list(0), each=total_nodes),
            # Trust manager data
            reputation = rep(1, each=total_nodes),
            ill_reputed_nodes = c(),
            final_trust = rep(1, each=total_nodes)
        )
    )
}

# Create the set of reports that will describe each of the nodes
create_report_set <- function(total_nodes) {
    fill_data = rep(0, total_nodes * total_nodes * 4)
    return(array(fill_data, c(total_nodes, total_nodes, 4)))
}

# Create graphs on each of the nodes
graph_node_data <- function(total_nodes, network, folder) {
    dir.create(sprintf("./graphs/%s", folder), showWarnings=FALSE)
    for(i in seq(1, total_nodes)) {
	cat(sprintf("Node: %4d\tQR: %f",
	    i, network$QR[[i]][[1]]
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
	    sprintf("S: %d\tC: %d\tFinal QR: %f\tRep: %f",
                network$service[[i]],
                network$capability[[i]],
	        head(network$QR[[i]], 1),
	        network$reputation[[i]]),
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

# Produce a line chart of data on a particular node
graph_single_node <- function(network, node_id) {
    plot(
        rev(network$QR[[node_id]]),
        type="l",
        xlab="Number of Recommendations",
        ylab="Quality of Recommendation",
        xlim=range(0, length(network$QR[[node_id]])),
        ylim=range(-1.5, 1.5),
        main=sprintf("Node %d Quality of Recommendation", node_id)
    )
    text(
        length(network$QR[[node_id]]) / 2,
        1.5,
        sprintf("S: %d\tC: %d\tNote acc.: %f\tFinal QR: %f\tRep: %f",
            network$service[[node_id]],
            network$capability[[node_id]],
            network$accurate_note_take[[node_id]],
            head(network$QR[[node_id]], 1),
            network$reputation[[node_id]]),
        cex=0.8
    )
    if(network$malicious[[node_id]]) {
        text(
            length(network$QR[[node_id]]) / 2,
            -1.5,
            network$attack_type[[node_id]],
            cex=0.8
        )
    }
}

# Plot out the reputations of the nodes within the network
graph_reputations <- function(network) {
    plot(
        network$id,
        network$reputation,
        xlab="Node ID",
        ylab="Reputation",
        xlim=c(1, length(network$id)),
        ylim=c(-1.5, 1.5),
        main="Reputations of the Nodes"
    )
}

# Plot the most recently assigned QR of the nodes in the network
graph_final_qrs <- function(network) {
    final_qrs = c()
    for(i in 1:length(network$QR)) {
        final_qrs = c(final_qrs, head(network$QR[[i]], 1))
    }
    plot(
        network$id,
        final_qrs,
        xlab="Node ID",
        ylab="Final Quality of Recommendation",
        xlim=c(1, length(network$id)),
        ylim=c(-1.5, 1.5),
        main="Final QRs of the Nodes"
    )
}

# Plot the most recently assigned trust values of the nodes in the network
graph_final_trust <- function(network) {
    plot(
        network$id,
        network$final_trust,
        xlab="Node ID",
        ylab="Final Trust values",
        xlim=c(1, length(network$id)),
        ylim=c(-1.5, 1.5),
        main="Final Trust Values of the Nodes"
    )
}
