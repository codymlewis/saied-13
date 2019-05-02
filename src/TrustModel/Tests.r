#!/usr/bin/env Rscript

# Unit tests for the program
#
# Author: Cody Lewis
# Date: 2019-05-02

library('testthat')

source("Report.r")
source("Node.r")

# Test the report functionality
test.report <- function() {
    r = Report(service=50, capability=50, time=0, note=1)
    report.check(r, 50, 50, 0, 1)
    d = report.distance(r, 100, 100, 100, 100, 1)
    expect_that(d, equals(100))
    w = report.weigh(r, d, 0.7, 0.7, 0)
    expect_that(w, equals(3.234477e-16))
}

report.check <- function(report, service, capability, time, note) {
    expect_that(report$service, equals(service))
    expect_that(report$capability, equals(capability))
    expect_that(report$time, equals(time))
    expect_that(report$note, equals(note))

}

# Test the node functionality
test.node <- function() {
    n <- Node(id=1, service=1, capability=1, noteacc=1, QR=1)
    node.check(n, 1, 1, 1, 1, 1)
    n1 <- Node(id=2, service=100, capability=100, noteacc=1, QR=1)
    node.check(n1, 2, 100, 100, 1, 1)
    n2 <- Node(id=3, service=100, capability=1, noteacc=1, QR=1)
    node.check(n2, 3, 100, 1, 1, 1)
    n$make.report(n1, 50, 50, 0)
    n1_report = n1$reports[[1]]
    report.check(n1$reports[[1]], 50, 50, 0, 1)
    n1$make.report(n, 50, 50, 0)
    report.check(n$reports[[1]], 50, 50, 0, -1)
}

node.check <- function(node, id, service, capability, noteacc, QR) {
    expect_that(node$id, equals(id))
    expect_that(node$service, equals(service))
    expect_that(node$capability, equals(capability))
    expect_that(node$noteacc, equals(noteacc))
    expect_that(node$QR, equals(QR))
}

test <- function() {
    cat("-------------- Performing Tests --------------\n")
    test.report()
    cat(".")
    test.node()
    cat(".")
    cat("\n")
    cat("------------- Test Cases Passed --------------\n")
    cat("Build Succeeded\n")
}

test()