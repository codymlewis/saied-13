#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2019-01-16
# Description:
# Creates data structures that the trust manager manages
# TODO:
# Plot trust values over time
# Plot QR values over time

library(scatterplot3d)
library(ggplot2)

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
    ids = seq(1, total_nodes)
    constrained_nodes = sample(ids, constrained * total_nodes)
    service = rep(100, each=total_nodes)
    service[constrained_nodes] = floor(runif(length(constrained_nodes), min=1, max=S_MAX))
    capability = rep(100, each=total_nodes)
    capability[constrained_nodes] = floor(runif(length(constrained_nodes), min=1, max=C_MAX))
    poor_witness_nodes = sample(ids, poor_witnesses * total_nodes)
    accurate_note_take = rep(1, each=total_nodes)
    accurate_note_take[poor_witness_nodes] = runif(length(poor_witness_nodes))
    return(
        list(
            # Basic node data
            id = ids,
            service = service,
            capability = capability,
            accurate_note_take = accurate_note_take,
            QR = rep(list(1), each=total_nodes),
            time_QR = rep(list(time), each=total_nodes),
            latest_qrs = rep(1, each=total_nodes),
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

# Produce a line chart of data on a particular node
graph_single_node <- function(network, node_id) {
    data = data.frame(
        recommendations = 1:length(network$QR[[node_id]]),
        QRs = rev(network$QR[[node_id]])
    )
    ggplot(data=data, aes(x=recommendations, y=QRs)) +
        geom_line(aes(colour=`if`(network$malicious[[node_id]], "red", "blue"))) +
        labs(
            title=sprintf("Node %d Quality of Recommendation", node_id),
            subtitle=sprintf(
                "S: %d\tC: %d\tNote acc.: %f\tFinal QR: %f\tRep: %f",
                network$service[[node_id]],
                network$capability[[node_id]],
                network$accurate_note_take[[node_id]],
                head(network$QR[[node_id]], 1),
                network$reputation[[node_id]]
            ),
            x="Number of Recommendations",
            y="Quality of Recommendation",
            caption=`if`(
                network$malicious[[node_id]],
                get_attack_name(network$attack_type[[node_id]]),
                NULL
            )
        ) +
        y_limit() +
        theme(legend.position = "none")
}

# Plot out the reputations of the nodes within the network
graph_reputations <- function(network) {
    data = data.frame(
        id = network$id,
        reputation = network$reputation,
        malicious_state = ifelse(network$malicious, "Malicious", "Non-Malicious")
    )
    ggplot(data=data, aes(x=id, y=reputation)) +
        geom_point(aes(colour=malicious_state)) +
        malicious_indicator() +
        labs(title="Reputations of the Nodes in the Network", colour=NULL) +
        y_limit()
}

# Plot the most recently assigned QR of the nodes in the network
graph_final_qrs <- function(network) {
    final_qrs = c()
    for(i in 1:length(network$QR)) {
        final_qrs = c(final_qrs, head(network$QR[[i]], 1))
    }
    data = data.frame(
        id = network$id,
        final_qrs = final_qrs,
        malicious_state = ifelse(network$malicious, "Malicious", "Non-Malicious")
    )
    ggplot(data=data, aes(x=id, y=final_qrs)) +
        geom_point(aes(colour=malicious_state)) +
        malicious_indicator() +
        labs(
            title="Final Quality of Recommendations of the Nodes",
            x="Node ID",
            y="Final Quality of Recommendation",
            colour=NULL
        ) +
        y_limit()
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
        main="Final Trust Values of the Node Services",
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

malicious_indicator <- function() {
    return(scale_color_manual(breaks=c("Malicious", "Non-Malicious"), values=c("red", "blue")))
}

y_limit <- function() {
    return(ylim(c(-1.1, 1.1)))
}

