#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2019-01-11
# Description
# Implementation of few of the standard attack
# of a trust model for the simulator

# Make the report worse than it should be
bad_mouth <- function() {
    -1
}

# Make the report better than it should be
good_mouth <- function() {
    1
}

# On a set out interval of 30 reports, change between good
# mouthing and bad mouthing
on_off <- function(is_bad_mouthing, timer) {
    if(is_bad_mouthing) {
    	report = bad_mouth()
    } else {
    	report = good_mouth()
    }
}
