#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2019-01-16
# Description:
# Creates data structures that the trust manager manages

library(ggplot2)

# Report matrix index
SERVICE_INDEX <- 1
CAPABILITY_INDEX <- 2
NOTE_INDEX <- 3
TIME_INDEX <- 4

SERVICES <- c(16, 33, 50, 66, 83, 100)

TARGET_GROUP <- seq(1, 30)

NODE_MON_ID <- 200 # ID of the node to monitor
TRUST_INDEX <- 5 # Index of trust values for the nodemon matrix

# Get the service requirements of a random service
get_random_service <- function() {
    return(SERVICES[[floor(runif(1, 1, length(SERVICES) + 1))]])
}

# Create the IoT network
create_network <- function(total_nodes, malicious_percent, time, S_max, C_max,
                           poor_witnesses, constrained, number_of_transactions) {
    target_group = TARGET_GROUP
    ids = seq(length(target_group) + 1, total_nodes)
    constrained_nodes = sample(ids, constrained * total_nodes)
    constrained_status = rep(FALSE, total_nodes)
    constrained_status[constrained_nodes] = rep(TRUE, length(constrained_nodes))
    service = rep(100, each=total_nodes)
    non_target_contexts = c(1:44, 56:S_MAX - 1)
    service[constrained_nodes] = sample(
        non_target_contexts, length(constrained_nodes), replace=TRUE
    )
    service[target_group] = floor(runif(length(target_group), min=45, max=55))
    capability = rep(100, each=total_nodes)
    capability[constrained_nodes] = sample(
        non_target_contexts, length(constrained_nodes), replace=TRUE
    )
    capability[target_group] = floor(runif(length(target_group), min=45, max=55))
    poor_witness_nodes = sample(ids, poor_witnesses * total_nodes)
    accurate_note_take = rep(1, each=total_nodes)
    accurate_note_take[poor_witness_nodes] = runif(length(poor_witness_nodes))
    ids = c(target_group, ids)
    return(
        list(
            # Basic node data
            id = ids,
            service = service,
            capability = capability,
            accurate_note_take = accurate_note_take,
            constrained = constrained_status,
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

decay_network <- function(network) {
    for(i in 1:length(network)) {
        if(network$constrained[i] && runif(1) < 0.1) {
            network$capability[i] = network$capability[i] - 1
        }
    }
    return(network)
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

graph_target_group_trust <- function(network, transactions) {
    target_group = TARGET_GROUP
    normal_group = head(
        network$id[!network$id %in% TARGET_GROUP], length(target_group)
    )
    target_group_trusts = rep(0, each=transactions)
    normal_group_trusts = rep(0, each=transactions)
    for(i in 1:transactions) {
        target_group_trusts[i] = mean(network$trust[target_group, i])
        normal_group_trusts[i] = mean(network$trust[normal_group, i])
    }
    trusts = c(target_group_trusts, normal_group_trusts)
    is_target_group = c(rep("Target Group", each=transactions), rep("Normal Group", each=transactions))
    data = data.frame(
        trusts=trusts,
        transactions = c(1:transactions, 1:transactions),
        is_target_group = is_target_group
    )
    ggplot(data=data, aes(x=transactions, y=trusts, group=is_target_group)) +
        geom_line(aes(color=is_target_group)) +
        labs(
            title="Average Trust of the Nodes Over Time",
            x="Number of Transactions",
            y="Average Trust Value",
            colour = NULL
        ) +
        target_indicator() +
        y_limit() +
        theme(legend.position = "bottom")
}

# Provide a limit on the y-axis for ggplot
y_limit <- function() {
    return(ylim(c(-1.1, 1.1)))
}

target_indicator <- function() {
    return(
        scale_color_manual(
            breaks=c("Target Group", "Normal Group"),
            values=c("green", "purple")
        )
    )
}
