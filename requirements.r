#!/usr/bin/env Rscript

r = getOption("repos")
r["CRAN"] = "https://cran.csiro.au/"
options(repos = r)
rm(r)

install.packages(c("shiny", "optigrab", "plotly", "Rcpp", "ggplot2", "testthat", "optparse"))
