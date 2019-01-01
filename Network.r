#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2019-01-01
# Description:
# An IoT network simulator

main <- function() {
    net = data.frame(
		id = seq(1, 200),
		energy = rep(100, each=200),
		QR = rep(1, each=200),
		services = rep(1, each=200),
		malicious = floor(runif(200, min=0, max=2)),
		reputation = rep(1, each=200),
		R_QR = runif(200)
    )
    print("The network is:")
    print(net)
}

main()
