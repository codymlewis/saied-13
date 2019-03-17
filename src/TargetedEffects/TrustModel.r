#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2018-12-28
# Description:
# A simulation of the trust model described in
# http://people.cs.vt.edu/~irchen/5984/pdf/Saied-CS14.pdf

library("Rcpp")

source("TrustManager.r")
source(sprintf("%sAttacks.r", ROOT))
source(sprintf("%sTrustModel.r", ROOT))

# Run through the system operations
run <- function(lambda, theta, eta, total_nodes, malicious_percent,
                phases, folder, attack_type, poor_witnesses, constrained) {
    time = 1
    network = create_network(total_nodes, malicious_percent, time,
                             S_MAX, C_MAX, poor_witnesses, constrained, phases)
    network$attack_type = assign_attack_types(network$attack_type, malicious_percent,
                                  total_nodes, attack_type)
    R = create_report_set(total_nodes)
    nodemon_data = create_nodemon_matrix(phases)
    end_phases = phases
    for(i in 1:phases) {
        cat(sprintf("Transaction: %d\n", i))
        R = initialize(network, R, time, lambda, theta, eta)
        if((i %% 30) == 0) {
            time = time + 1
        }
        cs_targets = c(floor(runif(1, min=1, max=C_MAX - 1)), get_random_service())
        result = post_init(network, lambda, theta, eta, R,
                           time, total_nodes, cs_targets, i)
        R = result[[1]]
        network = result[[2]]
        if(is.na(result[[3]])) {
            end_phases = i
            break
        }
        nodemon_data[i, ] = result[[3]]
    }
    print("Ill Reputed Nodes")
    print(network$ill_reputed_nodes)
    attack_name = get_attack_name(attack_type)
    dir.create(sprintf("./graphs/%s", REPUTATION_THRESHOLD),
                       showWarnings=FALSE)
    dir.create(sprintf("./graphs/%s/%s", REPUTATION_THRESHOLD, attack_name),
               showWarnings=FALSE)
    dir.create(sprintf("./graphs/%s/%s/%s", REPUTATION_THRESHOLD, attack_name,
                       folder), showWarnings=FALSE)
    graph_target_group_trust(network)
    ggsave(file = sprintf("./graphs/%s/%s/%s/Trust_Groups.png", REPUTATION_THRESHOLD, attack_name, folder))
}
