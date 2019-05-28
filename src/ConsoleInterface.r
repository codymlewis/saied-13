#!/usr/bin/env Rscript

# The main console interface fo the trust model
#
# Author: Cody Lewis
# Date: 2019-05-03

library("optparse")

source("TrustModel.r")
source("Functions.r")

# Find the malicious typing specified in the arguments
find.malicious.type <- function(opt) {
    malicious.type = ""
    if(opt$bad_mouth) {
        malicious.type = paste(malicious.type, "bm", sep="")
    } else if(opt$good_mouth) {
        malicious.type = paste(malicious.type, "gm", sep="")
    } else if(opt$on_off) {
        malicious.type = paste(malicious.type, "oo", sep="")
    }
    if(opt$service_set) {
        malicious.type = paste(malicious.type, "ss", sep="")
    }
    if(opt$capability_set) {
        malicious.type = paste(malicious.type, "cs", sep="")
    }
    if(opt$time_decay) {
        malicious.type = paste(malicious.type, "td", sep="")
    }

    return(malicious.type)
}

# The main program flow
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
        make_option(c("--malicious"), type="double", default=0.1,
                    help="Percentage of malicious nodes to start with [default %default]"),
        make_option(c("--malicious_start"), type="double", default=0,
                    help="Percentage of malicious reporting nodes to start with [default %default]"),
        make_option(c("--malicious_end"), type="double", default=1,
                    help="Percentage of malicious reporting nodes to end with [default %default]"),
        make_option(c("--malicious_jump"), type="double", default=0.05,
                    help="Percentage of malicious reporting nodes to increment by [default %default]"),
        make_option(c("--reputation", "-r"), type="double", default=-1,
                    help="Reputation threshold, nodes in the network that fall below this are no longer considered in the network"),
        make_option(c("--targeted", "-ta"), action="store_true", default=FALSE,
                    help="Analyze the targeted effects of an attack"),
        make_option(c("--alt", "-a"), action="store_true", default=FALSE,
                    help="Perform an alternate form of calculating the trust values."),
        make_option(c("--time_change", "-tc"), type="integer", action="store", default=60,
                    help="The number epochs to increment the time at. [default %default]")
    )
    parser <- OptionParser(usage="%prog [options]", option_list=option_list)
    args <- parse_args(parser, positional_arguments=0)
    opt <- args$options

    type.malicious = find.malicious.type(opt)
    type.calc = `if`(opt$alt, ALT1, 0)

    dir.create("./graphs", showWarnings=FALSE)
    dir.create(sprintf("./graphs/%f", opt$reputation), showWarnings=FALSE)
    dir.create(sprintf("./graphs/%f/%s", opt$reputation, type.malicious), showWarnings=FALSE)
    for(percent.malicious.reporters in seq(opt$malicious_start, opt$malicious_end, by=opt$malicious_jump)) {
        tm <- TrustManager(eta=opt$eta, lambda=opt$lambda, theta=opt$theta, service.max=100,
                           capability.max=100, reputation.threshold=opt$reputation, QR.initial=1)
        tm$init(
            number.nodes=opt$total_nodes,
            percent.constrained=opt$constrained,
            percent.poorwitness=opt$poor_witnesses,
            percent.malicious=opt$malicious,
            percent.malicious.reporter=percent.malicious.reporters,
            type.malicious=type.malicious,
            targeted=opt$targeted,
            type.calc=type.calc
        )
        epochs.total <- opt$transactions
        time.current <- 0
        cat(sprintf("Performing %d transactions in the network, with %f%% %s\n", epochs.total, percent.malicious.reporters * 100, type.malicious))
        for(epoch in 1:epochs.total) {
            if((epoch %% opt$time_change) == 0) {
                time.current <- time.current + 1
            }
            tm$phase(opt$total_nodes * 5, time.current)
            cat.progress(epoch, epochs.total, prefix=sprintf("%d/%d epochs completed", epoch, epochs.total))
        }
        dir.create(sprintf("./graphs/%f/%s/%f", opt$reputation, type.malicious, percent.malicious.reporters * 100), showWarnings=FALSE)
        plot.nodes(c(tm$nodes[[tm$id.nodemon.normal]], tm$nodes[[tm$id.nodemon.malicious]]))
        graph.save(sprintf("%f/%s/%f/qr_changes.png", opt$reputation, type.malicious, percent.malicious.reporters * 100))
        plot.trust(tm$nodes)
        graph.save(sprintf("%f/%s/%f/trust.png", opt$reputation, type.malicious, percent.malicious.reporters * 100))
        plot.QRs.final(tm$nodes)
        graph.save(sprintf("%f/%s/%f/final_qrs.png", opt$reputation, type.malicious, percent.malicious.reporters * 100))
        if(opt$targeted) {
            plot.trust.targeted(tm$nodes, epochs.total)
            graph.save(sprintf("%f/%s/%f/targeted_trust.png", opt$reputation, type.malicious, percent.malicious.reporters * 100))
        }
    }
}

main()
