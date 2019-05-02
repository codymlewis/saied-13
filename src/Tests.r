#!/usr/bin/env Rscript

library('testthat')

source("TrustModelv2/Report.r")


test.report <- function() {
    r = Report(service=50, capability=50, time=0, note=1)
    expect_that(r$service, equals(50))
    expect_that(r$capability, equals(50))
    expect_that(r$time, equals(0))
    expect_that(r$note, equals(1))
    d = report.distance(r, 100, 100, 100, 100, 1)
    expect_that(d, equals(100))
    w = report.weigh(r, d, 0.7, 0.7, 0)
    expect_that(w, equals(3.234477e-16))
}

test <- function() {
    cat("-------------- Performing Tests --------------\n")
    test.report()
    cat(".")
    cat("\n")
    cat("------------- Test Cases Passed --------------\n")
    cat("Build Succeeded\n")
}

test()
