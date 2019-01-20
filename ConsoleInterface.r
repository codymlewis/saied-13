#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2019-01-12
# The console interface for the trust model simulator

source("TrustModel.r")

# State how to use the program
help <- function() {
    paste(
    	"Run with arguments:",
	"--help | -h\t\t\t\tGet this help message",
	"--theta | -t <theta>\t\t\tValue of theta, indicates memory of the system",
	"--lambda | -l <lambda>\t\t\tValue of lambda, indicates memory of the system",
	"--eta | -e <eta>\t\t\tValue of eta, determines the amount of retained reports",
	"--total_nodes | -tn <total_nodes>\tThe number of nodes in the system",
	"--transactions | -tr <transactions>\tThe number of transactions to perform",
	"--bad-mouth\t\t\t\tMalicious nodes perform the bad mouth attack",
	"--good-mouth\t\t\t\tMalicious nodes perform the good mouth attack",
	"--on-off\t\t\t\tMalicious nodes perform the on-off attack",
	sep = "\n"
    )
}

main <- function() {
    args = commandArgs(trailingOnly=TRUE)
    theta=lambda=eta=total_nodes=0
    phases = 20
    if(length(args) == 0 || args[1] == "--help" || args[1] == "-h") {
    	cat(help(), "\n")
    	return(0)
    }
    attack_type = ""
    if("--bad-mouth" %in% args) {
    	attack_type = "bad mouther"
    	args = args[!args %in% "--bad-mouth"]
    } else if("--good-mouth" %in% args) {
    	attack_type = "good mouther"
    	args = args[!args %in% "--good-mouth"]
    } else if("--on-off" %in% args) {
    	attack_type = "on-off attacker"
    	args = args[!args %in% "--on-off"]
    } else {
    	attack_type = "random"
    }
    for(i in seq(1, length(args), by=2)) {
        if(args[i] %in% c("--theta", "-t")) {
            theta = as.numeric(args[i + 1])
        } else if(args[i] %in% c("--lambda", "-l")) {
            lambda = as.numeric(args[i + 1])
        } else if(args[i] %in% c("--eta", "-e")) {
            eta = as.numeric(args[i + 1])
	} else if(args[i] %in% c("--total-nodes", "-tn")) {
	    total_nodes = as.numeric(args[i + 1])
	} else if(args[i] %in% c("--transactions", "-tr")) {
	    phases = as.numeric(args[i + 1])
	}
    }
    dir.create("./graphs", showWarnings=FALSE)
    for(malicious_percent in seq(0, 9)) {
	print(sprintf("theta : %f, lambda : %f, eta : %f, total nodes: %d",
		      theta, lambda, eta, total_nodes))
	print(sprintf("Running %d transactions with %f%% malicious nodes...", phases, malicious_percent * 10))
	run(lambda, theta, eta, total_nodes, malicious_percent / 10, phases,
	    as.character(malicious_percent * 10), attack_type)
	print(sprintf("Placed the graphs in ./graphs/%d", malicious_percent * 10))
    }
    return(0)
}

main()
warnings()
