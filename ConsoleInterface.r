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
	"--malicious | -m <malicious>\t\tPercentage of malicious nodes in decimal form",
	"--phases | -p <phases>\t\tThe number passes through each of the phases",
	sep = "\n"
    )
}

main <- function() {
    args = commandArgs(trailingOnly=TRUE)
    theta=lambda=eta=total_nodes=malicious_percent=0
    phases = 20
    if(length(args) == 0 || args[1] == "--help" || args[1] == "-h") {
    	cat(help(), "\n")
    	return(0)
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
	} else if(args[i] %in% c("--malicious", "-m")) {
	    malicious_percent = as.numeric(args[i + 1])
	} else if(args[i] %in% c("--phases", "-p")) {
	    phases = as.numeric(args[i + 1])
	}
    }
    print(sprintf("theta : %f, lambda : %f, eta : %f", theta, lambda, eta))
    run(lambda, theta, eta, total_nodes, malicious_percent, phases)
    return(0)
}

main()
warnings()
