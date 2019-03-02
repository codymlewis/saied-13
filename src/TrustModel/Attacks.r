#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2019-01-11
# Description
# Implementation of few of the standard attack
# of a trust model for the simulator

ON_OFF_TOGGLE <- 30

# The attack types are primes so when multiplies they produce a unique
# composite number that becomes 0 if moduloed by the flag factor it.
NO_ATTACK_FLAG = 0
BAD_MOUTH_FLAG = 2
GOOD_MOUTH_FLAG = 3
ON_OFF_FLAG = 5
SERVICE_SET_FLAG = 7
CAPABILITY_SET_FLAG = 11
TIME_DECAY_FLAG = 13

ATTACKS <- c(BAD_MOUTH_FLAG)

# Assign the types of attackers for the malicious nodes
assign_attack_types <- function(attack_types, malicious_percent,
                                total_nodes, attack_type) {
    for(i in (total_nodes * (1 - malicious_percent)):total_nodes) {
        if(attack_type == "random") {
            choice = runif(1)
            attack_types[[i]] = rand_attack(choice)
        } else {
            attack_types[[i]] = attack_type
        }
    }
    return(attack_types)
}

# Return a random attack, each being equally likely to be chosen
rand_attack <- function(choice, i=1) {
    `if`(
        choice < (i / length(ATTACKS)),
        ATTACKS[[i]],
        rand_attack(choice, i + 1)
    )
}

# Get the attack name based on the attack type value
get_attack_name <- function(attack_type) {
    attack_name = ""
    if(attack_type %% BAD_MOUTH_FLAG == 0) {
        attack_name = sprintf("%s%s, ", attack_name, "Bad mouther")
    }
    if(attack_type %% GOOD_MOUTH_FLAG == 0) {
        attack_name = sprintf("%s%s, ", attack_name, "Good mouther")
    }
    if(attack_type %% ON_OFF_FLAG == 0) {
        attack_name = sprintf("%s%s, ", attack_name, "On-off Attacker")
    }
    if(attack_type %% SERVICE_SET_FLAG == 0) {
        attack_name = sprintf("%s%s, ", attack_name, "Service Setter")
    }
    if(attack_type %% CAPABILITY_SET_FLAG == 0) {
        attack_name = sprintf("%s%s, ", attack_name, "Capability Setter")
    }
    if(attack_type %% TIME_DECAY_FLAG == 0) {
        attack_name = sprintf("%s%s, ", attack_name, "Time Decayer")
    }
    return(attack_name)
}
