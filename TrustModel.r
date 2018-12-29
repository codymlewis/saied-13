#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2018-12-28
# Description:
# A simulation of the trust model described in http://people.cs.vt.edu/~irchen/5984/pdf/Saied-CS14.pdf

# Reports on a node, used to determine trustworthiness
reports <- data.frame(
    service = c(1),
    capability = c(1),
    note = c(1),
    time = c(1),
    max = 1
)

# Calculate a 1 dimensional distance between 2 points in a vector
find_dist <- function(vec, target, current_index) {
    abs(target - vec[current_index])
}

# Find the distance of between a nodes reports and the target conditions
report_dist <- function(node_reports, s_target, c_target, eta) {
    dS_max_sq = find_dist(node_reports$service, s_target, node_reports$max)**2
    dC_max_sq = find_dist(node_reports$capability, c_target, node_reports$max)**2
    S_max = node_reports$service[node_reports$max]
    C_max = node_reports$capability[node_reports$max]
    d <- c()
    for(j in seq(1, node_reports$max)) {
        d[j] = min(
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
    d
}

# The main thread, driver function
main <- function() {
    args = commandArgs(trailingOnly=TRUE)
    theta=lambda=eta=0
    for(i in seq(1, length(args), by=2)) {
        if(args[i] == "--theta" || args[i] == "-t") {
            theta = as.numeric(args[i + 1])
        } else if(args[i] == "--lambda" || args[i] == "-l") {
            lambda = as.numeric(args[i + 1])
        } else if(args[i] == "--eta" || args[i] == "-e") {
            eta = as.numeric(args[i + 1])
        }
    }
    print(sprintf("theta : %d, lambda : %d, eta : %d", theta, lambda, eta))
}

main()
