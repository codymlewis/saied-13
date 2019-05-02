#!/usr/bin/env Rscript

# Definition of the Trust Model
#
# Author: Cody Lewis
# Date: 2019-05-02

source("Report.r")
source("Node.r")
source("Plots.r")

# The Trust Manager class
TrustManager <- setRefClass(
    "TrustManager",
    fields=list(
        nodes="list",
        nodes.all="list",
        eta="numeric",
        theta="numeric",
        lambda="numeric",
        service.max="numeric",
        capability.max="numeric",
        reputation.threshold="numeric",
        QR.initial="numeric",
        reputed.ill="numeric"
    ),
    methods=list(
        init = function(number.nodes, percent.constrained, percent.poorwitness,
                              percent.malicious, type.malicious, targeted) {
            "Initialize the network to the specifications of the arguments"
            ids <- seq(1, number.nodes)
            if(targeted) {
                target.group = 1:30
                ids.constrained <- sample(ids, percent.constrained * number.nodes)
                service <- rep(service.max, number.nodes)
                services.nontarget = c(1:44, 56:service.max)
                service[ids.constrained] <- sample(services.nontarget, length(ids.constrained), replace=TRUE)
                service[target.group] <- round(runif(length(target.group), min=45, max=55))
                capability <- rep(capability.max, number.nodes)
                capabilities.nontarget = c(1:44, 56:capability.max)
                capability[ids.constrained] <- sample(capabilities.nontarget, length(ids.constrained), replace=TRUE)
                capability[target.group] <- round(runif(length(target.group), min=45, max=55))
            } else {
                # Assign constraint values
                ids.constrained <- sample(ids, percent.constrained * number.nodes)
                service = rep(service.max, number.nodes)
                service[ids.constrained] <- round(runif(length(ids.constrained), min=1, max=service.max))
                capability = rep(capability.max, number.nodes)
                capability[ids.constrained] <- round(runif(length(ids.constrained), min=1, max=capability.max))
            }
            # Assign note taking accuracy
            ids.poorwitness = sample(ids, percent.poorwitness * number.nodes)
            noteacc = rep(1.0, number.nodes)
            noteacc[ids.poorwitness] = runif(length(ids.poorwitness))
            # Assign the malicious node's ids
            ids.malicious = sample(ids, percent.malicious * number.nodes)
            # Create the nodes
            for(id in ids) {
                if(id %in% ids.malicious) {
                    if(type.malicious == "bm") {
                        nodes[[id]] <<- Node.BadMouther(id=id, service=service[id], capability=capability[id],
                                         noteacc=noteacc[id], QR=QR.initial, time.QR=0)
                    } else if(type.malicious == "bmss") {
                        nodes[[id]] <<- Node.BadMouther.ServiceSetter(id=id, service=service[id], capability=capability[id],
                                         noteacc=noteacc[id], QR=QR.initial, time.QR=0)
                    } else if(type.malicious == "bmcs") {
                        nodes[[id]] <<- Node.BadMouther.CapabilitySetter(id=id, service=service[id], capability=capability[id],
                                         noteacc=noteacc[id], QR=QR.initial, time.QR=0)
                    } else if(type.malicious == "bmtd") {
                        nodes[[id]] <<- Node.BadMouther.TimeDecayer(id=id, service=service[id], capability=capability[id],
                                         noteacc=noteacc[id], QR=QR.initial, time.QR=0)
                    } else if(type.malicious == "bmcstd") {
                        nodes[[id]] <<- Node.BadMouther.CapabilitySetter.TimeDecayer(id=id, service=service[id], capability=capability[id],
                                         noteacc=noteacc[id], QR=QR.initial, time.QR=0)
                    }
                } else {
                    nodes[[id]] <<- Node(id=id, service=service[id], capability=capability[id],
                                     noteacc=noteacc[id], QR=QR.initial, time.QR=0)
                }
                nodes.all[[id]] <<- nodes[[id]]
            }
        },
        info.gather = function(epochs, time.current) {
            "Induce random artificial intreractions between the nodes"
            for(epoch in 1:epochs) {
                client = nodes[[round(runif(1, min=1, max=length(nodes)))]]
                server = nodes[[round(runif(1, min=1, max=length(nodes)))]]
                service = round(runif(1, min=1, max=service.max))
                capability = round(runif(1, min=1, max=capability.max))
                client$make.report(server, service, capability, time.current)
            }
        },
        select.entity = function(target.service, target.capability, time.current) {
            "Perform the entity selection operations, and return the trusted list"
            trust = rep(0, length(nodes))
            for(node in nodes) {
                numerator = 0
                denominator = 0
                for(report in node$reports) {
                    dist = report.distance(report, target.service, target.capability, service.max, capability.max, eta)
                    if(dist < find.t(target.service, target.capability, service.max, capability.max)) {
                        weight = report.weigh(report, dist, lambda, theta, time.current)
                        numerator = numerator + weight * nodes[[report$issuer]]$QR[[1]] * report$note
                        denominator = denominator + weight
                    }
                }
                trust[[node$id]] = `if`(denominator == 0, 0, numerator / denominator)
                node$trust[[length(node$trust) + 1]] <- trust[[node$id]]
            }
            data.trust = data.frame(id=1:length(nodes), trust=trust)
            ids.trusted = data.trust[order(-data.trust$trust),]$id
            return(ids.trusted)
        },
        transaction = function(id.client, id.server, target.service, target.capability, time.current) {
            "Perform a transaction"
            server = nodes[[id.server]]
            nodes[[id.client]]$make.report(server, target.service, target.capability, time.current)
            report = server$reports[[length(server$reports)]]
            report$server <- TRUE
            client.note = report$note
            return(client.note)
        },
        update.QRs = function(id.client, client.note, target.service, target.capability, time.current) {
            "Update the QRs of the witness nodes"
            for(report in nodes[[id.client]]$reports) {
                r = -abs(report$note - client.note) + 1
                dist = report.distance(report, target.service, target.capability, service.max, capability.max, eta)
                if(dist < find.t(target.service, target.capability, service.max, capability.max)) {
                    C.client = report.weigh(report, dist, lambda, theta, time.current) * nodes[[id.client]]$QR[[1]]
                    QR.client.witness = C.client * r
                    node.witness = nodes[[report$issuer]]
                    numerator = 0
                    denominator = 0
                    for(index.QR in 1:length(node.witness$QR)) {
                        c.i = find.c.i(theta, node.witness$time.QR[[1]], node.witness$time.QR[[index.QR]])
                        numerator = numerator + c.i * node.witness$QR[[index.QR]] + QR.client.witness
                        denominator = denominator + c.i + abs(C.client)
                    }
                    node.witness$QR <- c(numerator / denominator, node.witness$QR)
                    node.witness$time.QR <- c(time.current, node.witness$time.QR)
                }
            }
        },
        update.reputation = function(id.server) {
            "Update the reputation of the server"
            node.server = nodes[[id.server]]
            reputation = 0
            for(report in node.server$reports) {
                if(report$server) {
                    c.i = find.c.i(theta, nodes[[report$issuer]]$time.QR[[1]], report$issuer.time.QR)
                    reputation = reputation + c.i * report$note * report$issuer.QR
                }
            }
            node.server$reputation <- reputation
            if(reputation < reputation.threshold) {
                nodes <<- nodes[!nodes %in% id.server]
            }
        },
        phase = function(epochs.bootstrap, time.current) {
            "Perform a single set of phases"
            info.gather(epochs.bootstrap, time.current)
            id.client = round(runif(1, min=1, max=length(nodes)))
            services = c(1, 16, 33, 50, 66, 82, 100)
            target.service = services[round(runif(1, min=1, max=length(services)))]
            target.capability = round(runif(1, min=1, max=capability.max))
            id.server = select.entity(target.service, target.capability, time.current)[[1]]
            client.note = transaction(id.client, id.server, target.service, target.capability, time.current)
            update.QRs(id.client, client.note, target.service, target.capability, time.current)
            update.reputation(id.server)
        }
    )
)

# Calculate c_i, a time decay value for Quality of Recommendation
find.c.i = function(theta, time.latest, time.QR) {
    return(theta**(time.latest - time.QR))
}
