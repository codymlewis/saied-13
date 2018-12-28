#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2018-12-28
# Description:
# A simulation of the trust model described in http://people.cs.vt.edu/~irchen/5984/pdf/Saied-CS14.pdf

# Reports on a node, used to determine trustworthiness
reports <- data.frame(
    service = c(),
    capability = c(),
    note = c(),
    time = c(),
    max = 0
)

# Calculate a 1 dimensional distance between 2 points in a vector
find_dist <- function(vec, target, current) {
    abs(vec[target] - vec[current])
}

# Find the distance of between a nodes reports and the target conditions
report_dist <- function(node_reports, s_target, c_target) {
    dS_max_sq = find_dist(node_reports$service, s_target, node_reports$max)**2
    dC_max_sq = find_dist(node_reports$capability, c_target, node_reports$max)**2
    d <- c()
    for j in [1:node_reports$max] {
        if node_reports$note[j] >= 0 {
            d[j] = min(
                sqrt((dS_max_sq + dC_max_sq) *
                     ((find_dist(node_reports$service, s_target, j)**2 / dS_max_sq) +
                      (find_dist(node_reports$capability, c_target, j)**2 / dC_max_sq))),
                sqrt(4)
            )
        } else {
            d[j] = min(sqrt(9), sqrt(25))
        }
    }
    d
}

# The main thread, driver function
main <- function() {
    print("No reports yet")
}

main()
