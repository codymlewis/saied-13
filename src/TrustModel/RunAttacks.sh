#!/bin/sh

for rep in -0.5 0 0.25 0.5; do
    ./ConsoleInterface.r --bad-mouth -r "$rep" & \
    ./ConsoleInterface.r --bad-mouth --service-set -r "$rep" & \
    ./ConsoleInterface.r --bad-mouth --capability-set -r "$rep" & \
    ./ConsoleInterface.r --bad-mouth --service-set --capability-set -r "$rep";
    ./ConsoleInterface.r --bad-mouth --time-decay -r "$rep" & \
    ./ConsoleInterface.r --bad-mouth --service-set --capability-set --time-decay -r "$rep" & \
    ./ConsoleInterface.r --bad-mouth --service-set --time-decay -r "$rep" & \
    ./ConsoleInterface.r --bad-mouth --capability-set --time-decay -r "$rep";
done
