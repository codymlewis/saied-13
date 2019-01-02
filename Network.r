#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2019-01-01
# Description:
# An IoT network simulator

TIME <- 0

# Return the note value based on how a proxy will perform on a transaction
take_note <- function(network, service_target, capability_target, proxy_id) {
	if(network$malicious[proxy_id]) {
		-1
	} else if(network$service[proxy_id] < service_target ||
			  network$capability[proxy_id] < capability_target) {
		0
	} else {
		1
	}
}

# Simulate a transaction, add a report entry based on that
transaction <- function(network, service_target, capability_target, proxy_id, reports) {
	j = length(reports$service) + 1
	reports$service[j] = network$service[proxy_id]
	reports$capability[j] = network$capability[proxy_id]
	reports$note[j] = take_note(network, service_target, capability_target, proxy_id)
	reports$time[j] = TIME
	reports
}

main <- function() {
	total_nodes = 200
	malicious_percent = 0.1
    network = data.frame(
		id = seq(1, total_nodes),
		energy = rep(100, each=total_nodes),
		QR = rep(1, each=total_nodes),
		service = floor(runif(200, min=1, max=101)),
		capability = floor(runif(200, min=1, max=101)),
		malicious = c(rep(FALSE, each=(total_nodes * (1 - malicious_percent))),
					  rep(TRUE, each=(total_nodes * malicious_percent))),
		reputation = rep(1, each=total_nodes),
		R_QR = runif(total_nodes)
    )
    R = list()
	for(i in seq(1, total_nodes)) {
		R[[i]] = list(c(), c(), c(), c())
		names(R[[i]]) <- c("service", "capability", "note", "time")
	}
    R[[2]] = transaction(network, 50, 70, 2, R[[2]])
    print(R)
}

main()
