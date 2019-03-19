#!/bin/sh

for rep in -1 -0.5 0 0.25 0.5; do
    ./ConsoleInterface.r --bad-mouth -r "$rep" --tr 1000 & \
    ./ConsoleInterface.r --bad-mouth --service-set -r "$rep" --tr 1000 & \
    ./ConsoleInterface.r --bad-mouth --capability-set -r "$rep" --tr 1000 & \
    ./ConsoleInterface.r --bad-mouth --service-set --capability-set -r "$rep" --tr 1000;
    ./ConsoleInterface.r --bad-mouth --time-decay -r "$rep" --tr 1000 & \
    ./ConsoleInterface.r --bad-mouth --service-set --capability-set --time-decay -r "$rep" --tr 1000 & \
    ./ConsoleInterface.r --bad-mouth --service-set --time-decay -r "$rep" --tr 1000 & \
    ./ConsoleInterface.r --bad-mouth --capability-set --time-decay -r "$rep" --tr 1000;
done
