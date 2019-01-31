#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2019-01-11
# Description
# Implementation of few of the standard attack
# of a trust model for the simulator

ON_OFF_TOGGLE <- 30
BAD_MOUTH_TEXT <- "bad mouther"
GOOD_MOUTH_TEXT <- "good mouther"
ON_OFF_TEXT <- "on-off attacker"
SERVICE_SET_TEXT <- "service setting attacker"
CAPABILITY_SET_TEXT <- "capability setting attacker"
SERVICE_CAPABILITY_SET_TEXT <- sprintf(
    "%s + %s + %s",
    SERVICE_SET_TEXT, CAPABILITY_SET_TEXT, BAD_MOUTH_TEXT
)
SERVICE_SET_M_TEXT <- sprintf("%s + %s", SERVICE_SET_TEXT, BAD_MOUTH_TEXT)
CAPABILITY_SET_M_TEXT <- sprintf("%s + %s", CAPABILITY_SET_TEXT, BAD_MOUTH_TEXT)
TIME_DECAY_TEXT <- "time decaying attacker"
TIME_DECAY_M_TEXT <- sprintf("%s + %s", TIME_DECAY_TEXT, BAD_MOUTH_TEXT)
SERVICE_SET_TIME_DECAY_TEXT <- sprintf(
    "%s + %s + %s",
    SERVICE_SET_TEXT, TIME_DECAY_TEXT, BAD_MOUTH_TEXT
)
CAPABILITY_SET_TIME_DECAY_TEXT <- sprintf(
    "%s + %s + %s",
    CAPABILITY_SET_TEXT, TIME_DECAY_TEXT, BAD_MOUTH_TEXT
)
SERVICE_CAPABILITY_SET_TIME_DECAY_TEXT <- sprintf(
    "%s + %s + %s + %s",
    SERVICE_SET_TEXT, CAPABILITY_SET_TEXT, TIME_DECAY_TEXT, BAD_MOUTH_TEXT
)
# TODO: Add in service-time and capability-time attacks
ATTACKS <- c(
    BAD_MOUTH_TEXT, GOOD_MOUTH_TEXT, ON_OFF_TEXT,
    SERVICE_SET_M_TEXT, CAPABILITY_SET_M_TEXT, TIME_DECAY_TEXT,
    SERVICE_CAPABILITY_SET_TEXT
)
NO_ATTACK_FLAG = "f"

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

# Always give a set value for the service context
service_set <- function() {
    return(50)
}

# Always give a set value for the capability context
capability_set <- function() {
    return(50)
}

# Say that a report is older than it really is
time_decay <- function(time) {
    return(time - 5)
}

# Assign the types of attackers for the malicious nodes
assign_attack_types <- function(attack_types, malicious_percent, total_nodes, attack_type) {
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
