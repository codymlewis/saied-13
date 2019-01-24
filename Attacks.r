#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2019-01-11
# Description
# Implementation of few of the standard attack
# of a trust model for the simulator

ATTACK_TYPE_COUNT <- 3
ON_OFF_TOGGLE <- 30

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
    } else {
    	return(good_mouth())
    }
}

# Assign the types of attackers for the malicious nodes
assign_attack_types <- function(attack_types, malicious_percent, total_nodes, attack_type) {
    for(i in (total_nodes * (1 - malicious_percent)):total_nodes) {
        if(attack_type == "random") {
            choice = runif(1)
            attack_types[[i]] = ifelse(
                choice < 1 / ATTACK_TYPE_COUNT,
                "bad mouther",
                ifelse(
                    choice < 2 / ATTACK_TYPE_COUNT,
                    "good mouther",
                    "on-off attacker"
                )
            )
        } else {
            attack_types[[i]] = attack_type
        }
    }
    return(attack_types)
}
