#!/bin/sh

./ConsoleInterface.r --bad-mouth  --tr 1000 & \
./ConsoleInterface.r --bad-mouth --service-set --tr 1000 & \
./ConsoleInterface.r --bad-mouth --capability-set --tr 1000 & \
./ConsoleInterface.r --bad-mouth --service-set --capability-set --tr 1000;
./ConsoleInterface.r --bad-mouth --time-decay --tr 1000 & \
./ConsoleInterface.r --bad-mouth --service-set --capability-set --time-decay --tr 1000 & \
./ConsoleInterface.r --bad-mouth --service-set --time-decay --tr 1000 & \
./ConsoleInterface.r --bad-mouth --capability-set --time-decay --tr 1000;
