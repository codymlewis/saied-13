#!/usr/bin/env python3

import matplotlib.pyplot as plt

def find_ci(theta, time1, timei):
    return theta**(time1 - timei)

def find_n_qr(qrs, times, theta):
    numerator = 0
    denominator = 0
    for qr, time in zip(qrs, times):
        ci = find_ci(theta, times[0], time)
        numerator += ci * qr
        denominator += ci
    return numerator / denominator

if __name__ == '__main__':
    QRS = [1]
    TIMES = [0]
    THETA = 0.7
    for i in range(0, 500):
        QRS.insert(0, find_n_qr(QRS, TIMES, THETA))
        TIMES.insert(0, i)

    QRS.reverse()
    TIMES.reverse()
    plt.plot(TIMES, QRS)
    plt.show()
