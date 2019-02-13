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
            c_i * node_qrs[[i]]
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
# calculate_reputation <- function(network, server, theta) {
#     sum = 0
#     for(j in seq(2, length(network$client_notes[[server]]))) {
#         client = network$clients[[server]][[j]]
#         sum = sum +
#             find_c_i(theta, network$time_QR[[client]][1],
#                      network$client_time_QR[[server]][[j]]) *
#             network$client_notes[[server]][[j]] *
#             network$client_QRs[[server]][[j]]
#     }
#     return(sum)
# }

main <- function() {
    s_target = c_target = 50
    s_j = 50
    eta = 1
    note = -1
    time = 1
    lambda = theta = 0.7
    for(c_j in 1:(C_MAX - 1)) {
        d = report_dist(c_j, s_j, c_target, s_target, eta, note, find_dist(S_MAX, s_target)**2, find_dist(C_MAX, c_target)**2, S_MAX, C_MAX)
        print(sprintf("distance: %f", d))
        w = find_weight(lambda, theta, note, time, d, time)
        print(sprintf("weight: %f", w))
    }
}

main()
