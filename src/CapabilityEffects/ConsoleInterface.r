#!/usr/bin/env Rscript

source("ReputationQR.r")

main <- function() {
    cat("Simulating the effects of varying capabilities on the system...\n")
    s_target = 50
    c_target = 50
    s_j = 50
    eta = 1
    node_note = -1
    lambda = 0.7
    theta = 0.7
    number_of_transactions = 300
    final_qrs = rep(1, each=100)
    reputations = matrix(
        rep(1, each=((number_of_transactions + 1) * 100)), nrow=100
    )
    for(c_j in 1:100) {
        node_qrs = c(1)
        node_qr_times = c(1)
        time = 1
        client_qrs = runif(number_of_transactions)
        client_notes = take_notes(c_j, c_target, client_qrs)
        for(transactions in 1:number_of_transactions) {
            if((transactions %% 30) == 0) {
                time = time + 1
            }
            d = report_dist(c_j, s_j, c_target, s_target, eta, node_note,
                            find_dist(S_MAX, s_target)**2,
                            find_dist(C_MAX, c_target)**2, S_MAX, C_MAX)
            w = find_weight(lambda, theta, node_note, time, d, time)
            qr = find_qr(w, client_notes[[transactions]], theta, time,
                         node_note, client_qrs[[transactions]], node_qrs,
                         node_qr_times)
            node_qrs = c(qr, node_qrs)
            node_qr_times = c(time, node_qr_times)
            reputations[c_j, transactions + 1] =
                calculate_reputation(
                    theta, client_notes, client_qrs,
                    transactions, time, node_qr_times
                )
        }
        final_qrs[[c_j]] = node_qrs[[1]]
        cat(
            sprintf(
                "Completed %d transactions for c_j: %d\n",
                number_of_transactions, c_j
            )
        )
    }
    dir.create("./graphs", showWarnings=FALSE)
    for(c_j in 1:100) {
        png(sprintf("graphs/%d_reputation_evolution.png", c_j))
        plot_reputation(reputations[c_j, ])
        dev.off()
    }
    plot_cap_qr(final_qrs)
    cat("Done. Created a few plots in the ./graphs directory\n")
}

main()
