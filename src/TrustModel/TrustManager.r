#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2019-01-16
# Description:
# Creates data structures that the trust manager manages

library(ggplot2)
library(gg3D)

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
create_network <- function(total_nodes, malicious_percent, time, S_max, C_max,
                           poor_witnesses, constrained, number_of_transactions) {
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
            trust = create_trust_matrix(total_nodes, number_of_transactions)
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

create_trust_matrix <- function(number_of_nodes, transactions) {
    fill_data = rep(1, number_of_nodes * transactions)
    return(matrix(fill_data, nrow=number_of_nodes, ncol=transactions))
}

# Produce a line chart of data on a particular node
graph_single_node <- function(network, node_id) {
    data = data.frame(
        recommendations = 1:length(network$QR[[node_id]]),
        QRs = rev(network$QR[[node_id]])
    )
    ggplot(data=data, aes(x=recommendations, y=QRs)) +
        geom_line(color=`if`(network$malicious[[node_id]], "red", "blue")) +
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

# Produce a line chart of data on a particular node
graph_two_nodes <- function(network, node_id_1, node_id_2) {
    malicious_states = c(
        rep(
            `if`(network$malicious[[node_id_1]], "Malicious", "Non-Malicious"),
            each=length(network$QR[[node_id_1]])
        ),
        rep(
            `if`(network$malicious[[node_id_2]], "Malicious", "Non-Malicious"),
            each=length(network$QR[[node_id_2]])
        )
    )
    data = data.frame(
        ids = c(
            rep(node_id_1, each=length(network$QR[[node_id_1]])),
            rep(node_id_2, each=length(network$QR[[node_id_2]]))
        ),
        recommendations = c(
            1:length(network$QR[[node_id_1]]),
            1:length(network$QR[[node_id_2]])
        ),
        QRs = c(rev(network$QR[[node_id_1]]), rev(network$QR[[node_id_2]])),
        malicious_state = malicious_states
    )
    ggplot(data=data, aes(x=recommendations, y=QRs, group=ids)) +
        geom_line(aes(colour=malicious_state)) +
        labs(
            title=sprintf(
                "Quality of Recommendation of a %s and %s Node",
                head(malicious_states, 1),
                tail(malicious_states, 1)
            ),
            x="Number of Recommendations",
            y="Quality of Recommendation"
        ) +
        malicious_indicator() +
        y_limit() +
        theme(legend.position = "none")
}

# Plot out the reputations of the nodes within the network
graph_reputations <- function(network) {
    data = data.frame(
        id = network$id,
        reputation = network$reputation[network$id],
        malicious_state = ifelse(network$malicious[network$id], "Malicious", "Non-Malicious")
    )
    ggplot(data=data, aes(x=id, y=reputation)) +
        geom_point(aes(colour=malicious_state)) +
        malicious_indicator() +
        labs(title="Reputations of the Nodes in the Network", colour=NULL) +
        y_limit() +
        theme(legend.position = "none")
}

# Plot the most recently assigned QR of the nodes in the network
graph_final_qrs <- function(network) {
    final_qrs = c()
    for(i in 1:max(network$id)) {
        final_qrs = c(final_qrs, head(network$QR[[i]], 1))
    }
    data = data.frame(
        id = network$id,
        final_qrs = final_qrs,
        malicious_state = ifelse(
            network$malicious[network$id], "Malicious", "Non-Malicious"
        )
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
        y_limit() +
        theme(legend.position = "none")
}

# Plot the most recently assigned trust values of the nodes in the network
graph_final_trust <- function(network) {
    divider = 40
    used_nodes = c(1:(nrow(network$trust) / divider) * divider, 1, nrow(network$trust))
    number_of_nodes = length(used_nodes)
    number_of_transactions = ncol(network$trust)
    ids = rep(0, each=number_of_nodes * number_of_transactions)
    transactions = rep(0, each=number_of_nodes * number_of_transactions)
    malicious_state = rep("", each=number_of_nodes * number_of_transactions)
    for(i in 1:number_of_nodes) {
        ids[(i - 1) * number_of_transactions + 1:number_of_transactions] = rep(
            used_nodes[[i]], each=number_of_transactions
        )
        transactions[(i - 1) * number_of_transactions + 1:number_of_transactions] = 1:number_of_transactions
        malicious_state[(i - 1) * number_of_transactions + 1:number_of_transactions] = rep(
            `if`(network$malicious[[used_nodes[[i]]]], "Malicious", "Non-Malicious"), each=number_of_transactions
        )
    }
    data = data.frame(
        trust=as.vector(network$trust[used_nodes, ]),
        transactions=transactions,
        ids=ids,
        malicious_state = malicious_state
    )
    ggplot(data=data, aes(x=transactions, y=trust, group=ids)) +
        geom_point(aes(colour=malicious_state)) +
        malicious_indicator() +
        labs(
            title="Trust Values of the Node Services",
            x="Number of Transactions",
            y="Trust Value",
            colour = NULL
        ) +
        y_limit() +
        theme(legend.position = "none")
}

# Form a plot of the QRs of all the nodes over time
graph_qr_over_time <- function(network) {
    divider = 40
    used_nodes = c(1:(nrow(network$trust) / divider) * divider, 1, nrow(network$trust))
    number_of_nodes = length(used_nodes)
    ids = c()
    transactions = c()
    malicious_state = c()
    next_transaction = 1
    for(i in 1:number_of_nodes) {
        number_of_transactions = length(network$QR[[used_nodes[[i]]]])
        ids[next_transaction:(next_transaction + number_of_transactions - 1)] = rep(
            used_nodes[[i]], each=number_of_transactions
        )
        transactions[next_transaction:(next_transaction + number_of_transactions - 1)] = 1:number_of_transactions
        malicious_state[next_transaction:(next_transaction + number_of_transactions - 1)] = rep(
            `if`(network$malicious[[used_nodes[[i]]]], "Malicious", "Non-Malicious"), each=number_of_transactions
        )
        next_transaction = next_transaction + number_of_transactions
    }
    QRs = c()
    for(i in 1:number_of_nodes) {
        QRs = c(QRs, unlist(rev(network$QR[[used_nodes[[i]]]])))
    }
    data = data.frame(
        QRs = QRs,
        transactions=transactions,
        ids=ids,
        malicious_state = malicious_state
    )
    ggplot(data=data, aes(x=transactions, y=QRs, group=ids)) +
        geom_line(aes(colour=malicious_state)) +
        malicious_indicator() +
        labs(
            title="Quality of Recommendations of the Nodes Over Time",
            x="Number of Transactions",
            y="Quality of Recommendation",
            colour = NULL
        ) +
        y_limit() +
        theme(legend.position = "none")
}

# Create a 3d plot of the monitored node data
graph_nodemon_data <- function(nodemon_data, node_id, is_malicious) {
    data = data.frame(
        x=nodemon_data[, TIME_INDEX],
        y=nodemon_data[, SERVICE_INDEX],
        z=nodemon_data[, TRUST_INDEX]
    )
    ggplot(data=data, aes(x=x, y=y, z=z)) +
        theme_void() +
        axes_3D() +
        stat_3D(color=`if`(is_malicious, "red", "blue")) +
        labs_3D(
            labs=c("Average Time Difference", "Average Service Target", "Trust Value"),
            hjust=c(0,1,1), vjust=c(1, 1, -0.2), angle=c(0, 0, 90)
        ) +
        axis_labs_3D(
            size=3,
        ) +
        theme(legend.position = "none")
}

# Colourise Malicious and non Malicious nodes for ggplot
malicious_indicator <- function() {
    return(scale_color_manual(breaks=c("Malicious", "Non-Malicious"), values=c("red", "blue")))
}

# Provide a limit on the y-axis for ggplot
y_limit <- function() {
    return(ylim(c(-1.1, 1.1)))
}
