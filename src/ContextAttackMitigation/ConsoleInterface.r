#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2019-01-12
# The console interface for the trust model simulator

library('optparse')

ROOT <- "../TrustModel/"

source("TrustModel.r")
source(sprintf("%sAttacks.r", ROOT))

find_attack_type <- function(options) {
    attack_type = BAD_MOUTH_FLAG
    if(options$good_mouth) {
        attack_type = GOOD_MOUTH_FLAG
    } else if(options$on_off) {
        attack_type = ON_OFF_FLAG
    }
    if(options$service_set) {
        attack_type = attack_type * SERVICE_SET_FLAG
    }
    if(options$capability_set) {
        attack_type = attack_type * CAPABILITY_SET_FLAG
    }
    if(options$time_decay) {
        attack_type = attack_type * TIME_DECAY_FLAG
    }
    return(attack_type)
}

main <- function() {
    option_list <- list(
        make_option(c("--bad_mouth"), action="store_true", default=FALSE,
                    help="Malicious nodes perform the bad mouthing attack"),
        make_option(c("--good_mouth"), action="store_true", default=FALSE,
                    help="Malicious nodes perform the good mouthing attack"),
        make_option(c("--on_off"), action="store_true", default=FALSE,
                    help="Malicious nodes perform the on-off attack"),
        make_option(c("--service_set"), action="store_true", default=FALSE,
                    help="Malicious nodes perform the service setting attack (along with mouthing, they always report a particular service value)"),
        make_option(c("--capability_set"), action="store_true", default=FALSE,
                    help="Malicious nodes perform the capability setting attack (along with mouthing, they always report a particular capability value)"),
        make_option(c("--time_decay"), action="store_true", default=FALSE,
                    help="Malicious nodes perform the time decay attack (along with mouthing, they always report a reduced time value)"),
        make_option(c("--theta", "-t"), type="double", default=0.7,
                    help="Value for theta [default %default]"),
        make_option(c("--lambda", "-l"), type="double", default=0.7,
                    help="Value for lambda [default %default]"),
        make_option(c("--eta", "-e"), type="integer", default=1,
                    help="Value for eta [default %default]"),
        make_option(c("--total_nodes"), type="integer", default=200,
                    help="Number of nodes in the simulated network [default %default]"),
        make_option(c("--transactions"), type="integer", default=300,
                    help="Number of transactions to simulate [default %default]"),
        make_option(c("--poor_witnesses", "-p"), type="double", default=0.2,
                    help="Percentage of poor witnesses in decimal form [default %default]"),
        make_option(c("--constrained", "-c"), type="double", default=0.5,
                    help="Percentage of constrained nodes in decimal form [default %default]"),
        make_option(c("--malicious_start"), type="double", default=0.5,
                    help="Percentage of malicious nodes to start with [default %default]"),
        make_option(c("--malicious_end"), type="double", default=9.5,
                    help="Percentage of malicious nodes to end with [default %default]"),
        make_option(c("--malicious_jump"), type="double", default=0.5,
                    help="Percentage of malicious nodes to increment by [default %default]"),
        make_option(c("--reputation", "-r"), type="double", default=-1,
                    help="Reputation threshold, nodes in the network that fall below this are no longer considered in the network")
    )
    parser <- OptionParser(usage="%prog [options]", option_list=option_list)
    args <- parse_args(parser, positional_arguments=0)
    opt <- args$options
    attack_type = find_attack_type(opt)
    REPUTATION_THRESHOLD = opt$reputation

    dir.create("./graphs", showWarnings=FALSE)
    for(malicious_percent in seq(opt$malicious_start, opt$malicious_end, by=opt$malicious_jump)) {
        cat(sprintf("theta : %f,\tlambda : %f,\teta : %d,\ttotal nodes: %d\tAttack type: %d\tconstrained: %f\tpoor witnesses: %f\n",
                      opt$theta, opt$lambda, opt$eta, opt$total_nodes, attack_type, opt$constrained, opt$poor_witnesses))
        cat(sprintf("Reputation Threshold: %d\n", REPUTATION_THRESHOLD))
        cat(sprintf("Running %d transactions with %f%% malicious nodes...\n",
                      opt$transactions, malicious_percent * 10))
        run(
            opt$lambda, opt$theta, opt$eta, opt$total_nodes, malicious_percent / 10, opt$transactions,
            as.character(malicious_percent * 10), attack_type, opt$poor_witnesses, opt$constrained
        )
        cat("Placed the graphs in the graphs folder\n")
    }
    quit("no")
}

main()
