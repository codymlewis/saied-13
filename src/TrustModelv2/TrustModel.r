#!/usr/bin/env Rscript

source("Report.r")
source("Node.r")

TrustManager <- setRefClass(
    "TrustManager",
    fields=list(
        nodes="list",
        eta="numeric",
        theta="numeric",
        lambda="numeric",
        service.max="numeric",
        capability.max="numeric",
        reputation.threshold="numeric",
        QR.initial="numeric"
    ),
    methods=list(
        init = function(number.nodes, percent.constrained, percent.poorwitness,
                              percent.malicious, type.malicious) {
            "Initialize the network to the specifications of the arguments"
            ids <- seq(1, number.nodes)
            # Assign constraint values
            ids.constrained <- sample(ids, percent.constrained * number.nodes)
            service = rep(service.max, number.nodes)
            service[ids.constrained] <- round(runif(length(ids.constrained), min=1, max=service.max))
            capability = rep(capability.max, number.nodes)
            capability[ids.constrained] <- round(runif(length(ids.constrained), min=1, max=capability.max))
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
                    }
                } else {
                    nodes[[id]] <<- Node(id=id, service=service[id], capability=capability[id],
                                     noteacc=noteacc[id], QR=QR.initial, time.QR=0)
                }
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
            trust = rep(0, length(nodes))
            for(node in nodes) {
                numerator = 0
                denominator = 0
                for(report in node$reports) {
                    dist = report.distance(report, target.service, target.capability, service.max, capability.max, eta)
                    weight = report.weigh(report, dist, lambda, theta, time.current)
                    numerator += weight * nodes[[report$issuer]]$QR[[1]] * report$note
                    denominator += weight
                }
                trust[[node$id]] = `if`(denominator == 0, 0, numerator / denominator)
            }
            data.trust = data.frame(id=1:length(nodes), trust=trust)
            ids.trusted = data.trust[order(-T$trust),]$id
            return ids.trusted
        },
        transaction = function(id.client, id.server, target.service, target.capability, time.current) {
            "Perform a transaction"
            nodes[[id.client]]$make.report(nodes[[id.server]], target.service, target.capability, time.current)
        },
        update.QRs = function(id.client, client.note, target.service, target.capability, time.current) {
            "Update the QRs of the witness nodes"
            for(report in nodes[[id.client]]$reports) {
                r = -abs(report$note - client.note) + 1
                dist = report.distance(report, target.service, target.capability, service.max, capability.max, eta)
                C.client = report.weigh(report, dist, lambda, theta, time.current) * nodes[[id.client]]$QR[[1]]
                QR.client.witness = C.client * r
                node.witness = nodes[[report$issuer]]
                numerator = 0
                denominator = 0
                for(index.QR in 1:length(node.witness$QR)) {
                    c.i = find.c.i(theta, node.witness$time.QR[[1]], node.witness$time.QR[[index.QR]])
                    numerator += c.i * node.witness$QR[[index.QR]] + QR.client.witness
                    denominator += c.i + abs(C.client)
                }
                node.witness$QR <- c(numerator / denominator, node.witness$QR)
                node.witness$time.QR <- c(time.current, node.witness$time.QR)
            }
        },
        update.reputation = function(id.server) {
            node.server = nodes[[id.server]]
            reputation = 0
            for(report in node.server$reports) {
                c.i = find.c.i(theta, nodes[[report$issuer]]$time.QR[[1]], report$issuer.time.QR)
                reputation += c.i * report$note * report$issuer.QR
            }
            node.server$reputation <- reputation
        }
    )
)

find.c.i = function(theta, time.latest, time.QR) {
    return(theta**(time.latest - time.QR))
}

test <- function() {
    tm <- TrustManager(service.max=100, capability.max=100, reputation.threshold=-1, QR.initial=1)
    tm$init(2, 1, 1, 1, "bmcs")
    print(tm)
    tm$info.gather(10, 0)
    print(tm)
}

test()
