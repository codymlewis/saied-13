#!/usr/bin/env Rscript

library("optparse")

source("TrustModel.r")
source("../Functions.r")

# The main program flow
main <- function() {
    option_list <- list(
        make_option(c("--bad_mouth"), action="store_true", default=FALSE,
                    help="Malicious nodes perform the bad mouthing attack"),
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
                    help="Reputation threshold, nodes in the network that fall below this are no longer considered in the network"),
        make_option(c("--targeted", "-ta"), action="store_true", default=FALSE,
                    help="Analyze the targeted effects of an attack")
    )
    parser <- OptionParser(usage="%prog [options]", option_list=option_list)
    args <- parse_args(parser, positional_arguments=0)
    opt <- args$options

    tm <- TrustManager(eta=opt$eta, lambda=opt$lambda, theta=opt$theta, service.max=100, capability.max=100, reputation.threshold=opt$reputation, QR.initial=1)
    tm$init(
        number.nodes=opt$total_nodes,
        percent.constrained=opt$constrained,
        percent.poorwitness=opt$poor_witnesses,
        percent.malicious=0.1,
        type.malicious="bm",
        targeted=opt$targeted
    )
    epochs.total <- opt$transactions
    time.current <- 0
    cat(sprintf("Performing %d transactions in the network\n", epochs.total))
    for(epoch in 1:epochs.total) {
        if((epoch %% 30) == 0) {
            time.current <- time.current + 1
        }
        tm$phase(50, time.current)
        cat.progress(epoch, epochs.total, prefix=sprintf("%d/%d epochs completed", epoch, epochs.total))
    }
    plot.nodes(tm$nodes)
    graph.save("qr_changes.png")
    plot.trust(tm$nodes)
    graph.save("trust.png")
    plot.QRs.final(tm$nodes)
    graph.save("final_qrs.png")
    if(opt$targeted) {
        plot.trust.targeted(tm$nodes, epochs.total)
        graph.save("targeted_trust.png")
    }
}

main()
