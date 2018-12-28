#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2018-12-28
# Description:
# A simulation of the trust model described in http://people.cs.vt.edu/~irchen/5984/pdf/Saied-CS14.pdf

# Report of a node, used to determine trustworthiness
reports <- data.frame(
    service = c(),
    capability = c(),
    node = c(),
    time = c()
)

# Calculate a 1 dimensional distance between 2 points in a vector
find_dist <- function(vec, target, current) {
    abs(vec[target] - vec[current])
}

# Find the distance of between a nodes reports and the target conditions
report_dist <- function(node_reports, s_target, c_target) {
    dS_max_sq = find_dist(node_report$service, s_target)**2
    dC_max_sq = find_dist(node_report$capability, c_target)**2
    min(
        sqrt((dS_max_sq + dC_max_sq) * ()
        ),
        sqrt(4)
    )
}

# The main thread, driver function
main <- function() {
    # reports <- c()
    # print(reports)
}

main()
