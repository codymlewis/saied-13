#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2019-01-11
# Description
# Implementation of few of the standard attack
# of a trust model for the simulator

ATTACK_TYPE_COUNT <- 3
ON_OFF_TOGGLE <- 30
BAD_MOUTH_TEXT <- "bad mouther"
GOOD_MOUTH_TEXT <- "good mouther"
ON_OFF_TEXT <- "on-off attacker"
SERVICE_SET_TEXT <- "service setting attacker"
CAPABILITY_SET_TEXT <- "capability setting attacker"
TIME_DECAY_TEXT <- "time decaying attacker"


# Make the report worse than it should be
bad_mouth <- function() {
    return(-1)
}

# Make the report better than it should be
good_mouth <- function() {
    return(1)
}

# On a set out interval, change between good mouthing and bad mouthing
on_off <- function(is_bad_mouthing) {
    if(is_bad_mouthing) {
    	return(bad_mouth())
    }
    return(good_mouth())
}

service_set <- function() {
    return(30)
}

capability_set <- function() {
    return(40)
}

time_decay <- function(time) {
    return(time - 5)
}

# Assign the types of attackers for the malicious nodes
assign_attack_types <- function(attack_types, malicious_percent, total_nodes, attack_type) {
    for(i in (total_nodes * (1 - malicious_percent)):total_nodes) {
        if(attack_type == "random") {
            choice = runif(1)
            attack_types[[i]] = ifelse(
                choice < 1 / ATTACK_TYPE_COUNT,
                BAD_MOUTH_TEXT,
                ifelse(
                    choice < 2 / ATTACK_TYPE_COUNT,
                    GOOD_MOUTH_TEXT,
                    ON_OFF_TEXT
                )
            )
        } else {
            attack_types[[i]] = attack_type
        }
    }
    return(attack_types)
}
