#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2019-01-16
# Description:
# Creates data structures that the trust manager manages

SERVICE_INDEX <- 1
CAPABILITY_INDEX <- 2
NOTE_INDEX <- 3
TIME_INDEX <- 4

SERVICES <- c(10, 25, 30, 50, 70, 75)

get_random_service <- function() {
    return(SERVICES[[floor(runif(1, 1, length(SERVICES) + 1))]])
}

# Create the IoT network
create_network <- function(total_nodes, malicious_percent, time,
                           S_max, C_max, poor_witnesses, constrained) {
    service = c(floor(runif(constrained * total_nodes, min=1, max=S_MAX)),
                rep(100, each=(1 - constrained) * total_nodes))
    capability = c(floor(runif(constrained * total_nodes, min=1, max=C_MAX)),
                   rep(100, each=(1 - constrained) * total_nodes))
    accurate_note_take = c(runif(poor_witnesses * total_nodes),
                           rep(1, each=(1 - poor_witnesses) * total_nodes))
    return(
        list(
            # Basic node data
            id = seq(1, total_nodes),
            service = service,
            capability = capability,
            accurate_note_take = accurate_note_take,
            R_QR = find_R_QR(service, capability, accurate_note_take),
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
            # Server based things
            client_notes = rep(list(0), each=total_nodes),
            clients = rep(list(0), each=total_nodes),
            # Trust manager data
            reputation = rep(1, each=total_nodes),
            ill_reputed_nodes = c()
        )
    )
}

find_R_QR <- function(service, capability, accurate_note_take) {
    sapply(1:length(accurate_note_take),
        function(i) {
            `if`(
                service[[i]] > capability[[i]],
                mean(c(service[[i]], accurate_note_take[[i]] * 100)) / 100,
                mean(c(capability[[i]], accurate_note_take[[i]] * 100)) / 100,
            )
        }
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
	     sprintf("S: %d\tC: %d\tR_QR: %f\tFinal QR: %f\tRep: %f",
                 network$service[[i]],
                 network$capability[[i]],
		 network$R_QR[[i]],
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
         sprintf("S: %d\tC: %d\tR_QR: %f\tFinal QR: %f\tRep: %f",
             network$service[[node_id]],
             network$capability[[node_id]],
             network$R_QR[[node_id]],
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
