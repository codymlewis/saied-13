#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2019-01-12
# The console interface for the trust model simulator

library('optigrab')

source("TrustModel.r")
source("Attacks.r")

# State how to use the program
help <- function() {
    paste(
        "Run with arguments:",
        "--help | -h\t\t\t\tGet this help message",
        "--theta | -t <theta>\t\t\tValue of theta, indicates memory of the system",
        "--lambda | -l <lambda>\t\t\tValue of lambda, indicates memory of the system",
        "--eta | -e <eta>\t\t\tValue of eta, determines the amount of retained reports",
        "--total_nodes | --tn <total_nodes>\tThe number of nodes in the system",
        "--transactions | --tr <transactions>\tThe number of transactions to perform",
        "--bad-mouth\t\t\t\tMalicious nodes perform the bad mouth attack",
        "--good-mouth\t\t\t\tMalicious nodes perform the good mouth attack",
        "--on-off\t\t\t\tMalicious nodes perform the on-off attack",
        "--service-set\t\t\t\tMalicious nodes perform the service set attack",
        "--capability-set\t\t\tMalicious nodes perform the capability set attack",
        "--service-capability-set\t\tMalicious nodes perform the service and capability set attack",
        "--service-set-time-decay\t\tMalicious nodes perform the service set and time attacks",
        "--capability-set-time-decay\t\tMalicious nodes perform the capability set and time attacks",
        "--service-capability-set-time-decay\tMalicious nodes perform the service and capability set and time attacks",
        "--time-decay\t\t\t\tMalicious nodes perform the time decay attack",
        "--malicious | -m <start> <end> <jump>\tThe range of percentages (n * 10) of malicious nodes there are to be",
        "--poor-witnesses | -p <poor_witnesses>\tThe percentage of poor witness nodes in decimal form",
        "--constrained | -c <constrained_nodes>\tPercentage of constrained nodes in decimal form",
        sep = "\n"
    )
}

main <- function() {
    if(opt_get(c("help", "h"), n=0)) {
        cat(help(), "\n")
        quit("no")
    }
    attack_type = BAD_MOUTH_FLAG
    attack_type = `if`(opt_get("bad-mouth", n=0), BAD_MOUTH_FLAG, attack_type)
    attack_type = `if`(opt_get("good-mouth", n=0), GOOD_MOUTH_FLAG, attack_type)
    attack_type = `if`(opt_get("on-off", n=0), ON_OFF_FLAG, attack_type)
    attack_type = `if`(opt_get("service-set", n=0), SERVICE_SET_FLAG * attack_type, attack_type)
    attack_type = `if`(opt_get("capability-set", n=0), CAPABILITY_SET_FLAG * attack_type, attack_type)
    attack_type = `if`(opt_get("service-capability-set", n=0), SERVICE_SET_FLAG * CAPABILITY_SET_FLAG * attack_type, attack_type)
    attack_type = `if`(opt_get("time-decay", n=0), TIME_DECAY_FLAG * attack_type, attack_type)
    attack_type = `if`(opt_get("service-capability-set-time-decay", n=0), SERVICE_SET_FLAG * CAPABILITY_SET_FLAG * TIME_DECAY_FLAG * attack_type, attack_type)
    attack_type = `if`(opt_get("service-set-time-decay", n=0), SERVICE_SET_FLAG * TIME_DECAY_FLAG * attack_type, attack_type)
    attack_type = `if`(opt_get("capability-set-time-decay", n=0), CAPABILITY_SET_FLAG * TIME_DECAY_FLAG * attack_type, attack_type)
    theta = as.numeric(opt_get(c("theta", "t"), default=0.7))
    lambda = as.numeric(opt_get(c("lambda", "l"), default=0.7))
    eta = as.numeric(opt_get(c("eta", "e"), default=1))
    total_nodes = as.numeric(opt_get(c("total-nodes", "tn"), default=200))
    phases = as.numeric(opt_get(c("transactions", "tr"), default=300))
    poor_witnesses = as.numeric(opt_get(c("poor-witnesses", "p"), default=0.2))
    constrained = as.numeric(opt_get(c("constrained", "c"), default=0.5))
    malicious_flow = as.numeric(opt_get(c("malicious", "m"), n=3, default=c(1, 9, 1)))
    dir.create("./graphs", showWarnings=FALSE)
    for(malicious_percent in seq(malicious_flow[[1]], malicious_flow[[2]], by=malicious_flow[[3]])) {
        cat(sprintf("theta : %f,\tlambda : %f,\teta : %d,\ttotal nodes: %d\tAttack type: %s\tconstrained: %f\tpoor witnesses: %f\n",
                      theta, lambda, eta, total_nodes, attack_type, constrained, poor_witnesses))
        cat(sprintf("Running %d transactions with %f%% malicious nodes...\n",
                      phases, malicious_percent * 10))
        run(
            lambda, theta, eta, total_nodes, malicious_percent / 10, phases,
            as.character(malicious_percent * 10), attack_type, poor_witnesses,
            constrained
        )
        cat("Placed the graphs in the graphs folder\n")
    }
    quit("no")
}

main()
