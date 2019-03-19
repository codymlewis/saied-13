#!/bin/sh

for rep in -0.5; do
    ./ConsoleInterface.r --bad-mouth --time-decay -r "$rep" --tr 1000 & \
    ./ConsoleInterface.r --bad-mouth --service-set --capability-set --time-decay -r "$rep" --tr 1000 & \
    ./ConsoleInterface.r --bad-mouth --service-set --time-decay -r "$rep" --tr 1000 & \
    ./ConsoleInterface.r --bad-mouth --capability-set --time-decay -r "$rep" --tr 1000;
done
