#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2018-12-28
# Description:
# A simulation of the trust model described in http://people.cs.vt.edu/~irchen/5984/pdf/Saied-CS14.pdf

RESTRICTED_REPORT = -1 # Marker showing that the report is restricted
time = as.numeric(Sys.time())

# Reports on a node, used to determine trustworthiness
reports <- data.frame(
    service = floor(runif(100, min=0, max=101)),
    capability = floor(runif(100, min=0, max=101)),
    note = floor(runif(100, min=-1, max=2)),
    time = floor(runif(100, min=time - (time / 20), max=time)),
    quality_of_recommendation = floor(runif(100, min=-1, max=2))
)

# Calculate a 1 dimensional distance between 2 points in a vector
find_dist <- function(vec, target, current_index) {
    abs(target - vec[current_index])
}

# Find the distance between a report's context and a target context
report_dist <- function(node_reports, s_target, c_target, eta, dS_max_sq, dC_max_sq, S_max, C_max, j) {
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
    d <- c()
    for(j in seq(1, length(node_reports$service))) {
        d[j] = report_dist(node_reports, s_target, c_target, eta, dS_max_sq, dC_max_sq, S_max, C_max, j)
        print(j)
        print(d[j])
        if(d[j] < t) {
            d[j] = RESTRICTED_REPORT
        }
    }
    d
}

find_s <- function(note_j) {
    (1 / 2) * (note_j**2 - note_j)
}

weigh_reports <- function(lambda, theta, node_reports, report_distances) {
    w = c()
    for(j in seq(1, length(node_reports$service))) {
        w[j] = (lambda ** report_distances[j]) *
            (theta ** ((find_s(node_reports$note[j]) + 1) * (time - node_reports$time[j])))
    }
    w
}

# Find the trust values of the proxies
compute_trust <- function(R, w) {
    T = c()
    for(i in seq(1, length(R))) {
        numerator = 0
        denominator = 0
        for(i in seq(1, length(w[i]))) {
            if(w[i][j] != RESTRICTED_REPORT) {
                numerator = numerator + (w[i][j] * R[i]$quality_of_recommendation[j] * R[i]$note[j])
                denominator = denominator + w[i][j]
            }
        }
        T[i] = numerator / denominator
    }
    T
}

# Select suitable entities for a target service
entity_selection <- function(lambda, theta, eta, R, s_target, c_target) {
    d = c()
    w = c()
    for(i in seq(1, length(R))) {
        d[i] = restrict_reports(R[i], s_target, c_target, eta)
        w[i] = ifelse(d[i] == RESTRICTED_REPORT,
                        RESTRICTED_REPORT,
                        weigh_reports(lambda, theta, R[i], d[i]))
    }
    T = compute_trust(R, w)
    print(T)
}

# The main thread, driver function
main <- function() {
    args = commandArgs(trailingOnly=TRUE)
    theta=lambda=eta=s_target=c_target=0
    for(i in seq(1, length(args), by=2)) {
        if(args[i] == "--theta" || args[i] == "-t") {
            theta = as.numeric(args[i + 1])
        } else if(args[i] == "--lambda" || args[i] == "-l") {
            lambda = as.numeric(args[i + 1])
        } else if(args[i] == "--eta" || args[i] == "-e") {
            eta = as.numeric(args[i + 1])
        } else if(args[i] == "--Starget" || args[i] == "-st") {
            s_target = as.numeric(args[i + 1])
        } else if(args[i] == "--Ctarget" || args[i] == "-ct") {
            c_target = as.numeric(args[i + 1])
        }
    }
    print(sprintf("theta : %d, lambda : %d, eta : %d, Starget : %d, Ctarget : %d", theta, lambda, eta, s_target, c_target))
    R = c()
    for(i in seq(1, 100)) {
        R[i] = reports
    }
    entity_selection(lambda, theta, eta, R, s_target, c_target)
}

main()
