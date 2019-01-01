#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2019-01-01
# Description:
# An IoT network simulator

main <- function() {
	total_nodes = 200
	malicious_percent = 0.1
    net = data.frame(
		id = seq(1, total_nodes),
		energy = rep(100, each=total_nodes),
		QR = rep(1, each=total_nodes),
		services = rep(1, each=total_nodes),
		malicious = c(rep(FALSE, each=(total_nodes * (1 - malicious_percent))), rep(TRUE, each=(total_nodes * malicious_percent))),
		reputation = rep(1, each=total_nodes),
		R_QR = runif(total_nodes)
    )
    print("The network is:")
    print(net)
}

main()
