#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2019-01-16
# Description:
# Creates data structures that the trust manager manages

library(scatterplot3d)

# Report matrix index
SERVICE_INDEX <- 1
CAPABILITY_INDEX <- 2
NOTE_INDEX <- 3
TIME_INDEX <- 4

SERVICES <- c(16, 33, 50, 66, 83, 100)

NODE_MON_ID <- 200 # ID of the node to monitor
TRUST_INDEX <- 5 # Index of trust values for the nodemon matrix

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
            service = c(
                floor(runif(constrained * total_nodes, min=1, max=S_MAX)),
                rep(100, each=(1 - constrained) * total_nodes)
            ),
            capability = c(
                floor(runif(constrained * total_nodes, min=1, max=C_MAX)),
                rep(100, each=(1 - constrained) * total_nodes)
            ),
            accurate_note_take = c(
                runif(poor_witnesses * total_nodes),
                rep(1, each=(1 - poor_witnesses) * total_nodes)
            ),
            QR = rep(list(1), each=total_nodes),
            time_QR = rep(list(time), each=total_nodes),
            # Attack based things
            malicious = c(
                rep(
                    FALSE,
                    each=ceiling(total_nodes * (1 - malicious_percent))
                ),
                rep(TRUE, each=ceiling(total_nodes * malicious_percent))
            ),
            attack_type = rep(NO_ATTACK_FLAG, each=total_nodes),
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
    fill_data = rep(RESTRICTED_REPORT, total_nodes * total_nodes * 4)
    return(array(fill_data, c(total_nodes, total_nodes, 4)))
}

# Create the matrix to store data of a monitored node
create_nodemon_matrix <- function(transactions) {
    fill_data = rep(0, transactions * 5)
    return(matrix(fill_data, nrow=transactions, ncol=5))
}

# Create graphs on each of the nodes
graph_node_data <- function(total_nodes, network, folder) {
    dir.create(sprintf("./graphs/%s", folder), showWarnings=FALSE)
    for(i in seq(1, total_nodes)) {
        cat(sprintf("Node: %4d\tQR: %f\n",
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
            main=sprintf("Node %d Quality of Recommendation", i),
            col=ifelse(
                network$malicious,
                "red",
                "blue"
            )
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
                get_attack_name(network$attack_type[[i]]),
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
        main=sprintf("Node %d Quality of Recommendation", node_id),
        col=`if`(network$malicious[[node_id]], "red", "blue")
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
    print(node_id)
    if(network$malicious[[node_id]]) {
        text(
            length(network$QR[[node_id]]) / 2,
            -1.5,
            get_attack_name(network$attack_type[[node_id]]),
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
        main="Reputations of the Nodes",
        col=ifelse(
            network$malicious,
            "red",
            "blue"
        )
    )
    malicious_legend(1, 1.5)
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
        main="Final QRs of the Nodes",
        col=ifelse(
            network$malicious,
            "red",
            "blue"
        )
    )
    malicious_legend(1, 1.5)
}

# Plot the most recently assigned trust values of the nodes in the network
graph_final_trust <- function(network) {
    plot(
        network$id,
        network$final_trust,
        xlab="Node ID",
        ylab="Final Trust values",
        xlim=c(1, length(network$id)),
        ylim=c(-1, 1),
        main="Final Trust Values of the Nodes",
        col=ifelse(
            network$malicious,
            "red",
            "blue"
        )
    )
    malicious_legend(1, 1)
}

# Create a 3d plot of the monitored node data
graph_nodemon_data <- function(nodemon_data, node_id, is_malicious) {
    p <- scatterplot3d(
        x = nodemon_data[, TIME_INDEX],
        y = nodemon_data[, SERVICE_INDEX],
        z = nodemon_data[, TRUST_INDEX],
        xlab = "Average Time Difference",
        ylab = "Average Service Target",
        zlab = "Trust Value",
        xlim = c(-5, 5),
        ylim = c(50, 70),
        zlim = c(-1, 1),
        main = sprintf("Trust Impact of a %sMalicious Node",
                       `if`(is_malicious, "", "Non-")),
        color=`if`(is_malicious, "red", "blue")
    )
}

graph_qr_gradient <- function(network) {
    gradients = rep(0, each=length(network$QR))
    for(i in 1:length(network$QR)) {
        gradients[[i]] = sum(network$QR[[i]]) / length(network$QR[[i]])
    }
    plot(
        x = network$id,
        y = gradients,
        xlab = "Node id",
        ylab = "Gradient of the Node's QR",
        ylim = c(-1.5, 1.5),
        main="QR Gradients of the Nodes",
        col=ifelse(
            network$malicious,
            "red",
            "blue"
        )
    )
    malicious_legend(1, 1.5)
}

# Add a legend indicating the symbols for malicious and non-malicious nodes
malicious_legend <- function(x, y) {
    legend(
        x,
        y,
        legend=c("Malicious", "Non-malicious"),
        col=c("red", "blue"),
        pch=c(1, 1),
        cex=0.8,
        box.lty=0
    )
}
