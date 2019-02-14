#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2019-02-13
# Description:
# Show the relationship between capability, QR, and Reputation

# The max values of service and capability respectively
S_MAX = 101
C_MAX = 101

# Get a scalar distance between 2 values
find_dist <- function(target, current) {
    return(abs(target - current))
}

# Find the distance between a report's context and a target context
report_dist <- function(c_j, s_j, c_target, s_target, eta, note,
			dS_max_sq, dC_max_sq, S_max, C_max) {
    shared_term = sqrt(
        (dS_max_sq + dC_max_sq) *
        (
         (find_dist(s_target, s_j)**2 /
          dS_max_sq) +
             (find_dist(c_target, c_j)**2 /
              dC_max_sq)
        )
    )
    unique_term = `if`(
        note,
        sqrt(
            (dS_max_sq + dC_max_sq) *
            (
             ((S_max - s_j) /
              (S_max - (s_target - eta)))**2 +
             (c_j /
              (c_target + eta))**2
            )
        ),
        sqrt(
            (dS_max_sq + dC_max_sq) *
            (
             ((C_max - c_j) /
              (C_max - (c_target - eta)))**2 +
             (s_j / (s_target + eta))**2
            )
        )
    )
    return(min(shared_term, unique_term))
}

find_s <- function(note_j) {
    return((1 / 2) * (note_j**2 - note_j))
}

# Find the weight of a report
find_weight <- function(lambda, theta, note, report_time, distance, time) {
    theta_exp = ((find_s(note) + 1) * (time - report_time))
    return((lambda ** distance) * (theta ** theta_exp))
}

# Give a value stating the significance of older occurances
find_c_i <- function(theta, t_1, t_i) {
    return(theta ** (t_1 - t_i))
}

# Find a quality of reccomendation for a given report weight
find_qr <- function(weight, client_note, theta, time, node_note,
                    client_qr, node_qrs, node_qr_times) {
    QR = 1
    C_F = weight * client_qr
    QRXF = C_F * (-abs(node_note - client_note))
    numerator=denominator=0
    numerator = sum(sapply(1:length(node_qrs),
        function(i) {
            c_i = find_c_i(theta, node_qr_times[[1]],
                           node_qr_times[[i]])
            c_i * node_qrs[[i]] + QRXF
        }
    ))
    denominator = sum(sapply(1:length(node_qrs),
        function(i) {
            c_i = find_c_i(theta, node_qr_times[[1]],
                           node_qr_times[[i]])
            c_i + abs(C_F)
        }
    ))
    QR = `if`(
        denominator == 0,
        0,
        numerator / denominator
    )
    if(QR < -1) {
        QR = -1
    } else if(QR > 1) {
        QR = 1
    }
    return(QR)
}

# Calculate the reputation of a server
calculate_reputation <- function(theta, client_note, client_qr, transactions,
                                 time, node_qr_times) {
    sum = 0
    for(i in 1:transactions) {
        sum = sum + find_c_i(theta, time, node_qr_times[[i]]) *
            client_note * client_qr
    }
    return(sum)
}

take_note <- function(c, c_target) {
    `if`(c < c_target, 0, 1)
}

main <- function() {
    s_target = 50
    c_target = 50
    s_j = 50
    eta = 1
    node_note = -1
    client_qr = 1
    lambda = theta = 0.7
    final_qrs = c()
    for(c_j in max((c_target - 38), 1):min((c_target + 38), C_MAX - 1)) {
        node_qrs = c(1)
        node_qr_times = c(1)
        time = 1
        client_note = take_note(c_j, c_target)
        for(transactions in 1:300) {
            if((transactions %% 30) == 0) {
                time = time + 1
            }
            d = report_dist(c_j, s_j, c_target, s_target, eta, node_note,
                            find_dist(S_MAX, s_target)**2,
                            find_dist(C_MAX, c_target)**2, S_MAX, C_MAX)
            w = find_weight(lambda, theta, node_note, time, d, time)
            qr = find_qr(w, client_note, theta, time, node_note, client_qr,
                         node_qrs, node_qr_times)
            node_qrs = c(qr, node_qrs)
            node_qr_times = c(time, node_qr_times)
        }
        final_qrs = c(final_qrs, node_qrs[[1]])
        print(sprintf("c_j: %d", c_j))
        print("QRs")
        print(node_qrs)
    }
    png("cap_qr.png")
    plot(
        x = max((c_target - 38), 1):min((c_target + 38), C_MAX - 1),
        y = final_qrs,
        xlab = "Capability Values",
        ylab = "Final QRs",
        main = "Capability vs. QR when Bad Mouthing a Good Service",
        col = "red"
    )
    text(
        18,
        0,
        sprintf("c_target: %d", c_target),
        cex = 0.8
    )
    dev.off()
}

main()
