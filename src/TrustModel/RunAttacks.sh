#!/bin/sh

# ./ConsoleInterface.r --bad-mouth & \
# ./ConsoleInterface.r --bad-mouth --service-set & \
# ./ConsoleInterface.r --bad-mouth --capability-set;
# ./ConsoleInterface.r --bad-mouth --service-set --capability-set & \
# ./ConsoleInterface.r --bad-mouth --time-decay & \
# ./ConsoleInterface.r --bad-mouth --service-set --capability-set --time-decay;
# ./ConsoleInterface.r --bad-mouth --service-set --time-decay & \
# ./ConsoleInterface.r --bad-mouth --capability-set --time-decay;

# ./ConsoleInterface.r --bad-mouth -r -0.5 & \
./ConsoleInterface.r --bad-mouth --service-set -r -0.5 -m 9 9 1 & \
./ConsoleInterface.r --bad-mouth --capability-set -r -0.5 -m 8 9 1;
./ConsoleInterface.r --bad-mouth --service-set --capability-set -r -0.5 -m 8 9 1 & \
./ConsoleInterface.r --bad-mouth --time-decay -r -0.5 -m 5 9 1 & \
./ConsoleInterface.r --bad-mouth --service-set --capability-set --time-decay -r -0.5 -m 7 9 1;
./ConsoleInterface.r --bad-mouth --service-set --time-decay -r -0.5 -m 6 9 1 & \
./ConsoleInterface.r --bad-mouth --capability-set --time-decay -r -0.5 -m 8 9 1;

./ConsoleInterface.r --bad-mouth -r 0 -m 4 9 1 & \
./ConsoleInterface.r --bad-mouth --service-set -r 0 -m 5 9 1 & \
./ConsoleInterface.r --bad-mouth --capability-set -r 0 -m 8 9 1;
./ConsoleInterface.r --bad-mouth --service-set --capability-set -r 0 -m 8 9 1 & \
./ConsoleInterface.r --bad-mouth --time-decay -r 0 -m 5 9 1 & \
./ConsoleInterface.r --bad-mouth --service-set --capability-set --time-decay -r 0 -m 7 9 1;
./ConsoleInterface.r --bad-mouth --service-set --time-decay -r 0 -m 7 9 1 & \
./ConsoleInterface.r --bad-mouth --capability-set --time-decay -r 0 -m 7 9 1;

./ConsoleInterface.r --bad-mouth -r 0.25 & \
./ConsoleInterface.r --bad-mouth --service-set -r 0.25 & \
./ConsoleInterface.r --bad-mouth --capability-set -r 0.25;
./ConsoleInterface.r --bad-mouth --service-set --capability-set -r 0.25 & \
./ConsoleInterface.r --bad-mouth --time-decay -r 0.25 & \
./ConsoleInterface.r --bad-mouth --service-set --capability-set --time-decay -r 0.25;
./ConsoleInterface.r --bad-mouth --service-set --time-decay -r 0.25 & \
./ConsoleInterface.r --bad-mouth --capability-set --time-decay -r 0.25;

./ConsoleInterface.r --bad-mouth -r 0.5 & \
./ConsoleInterface.r --bad-mouth --service-set -r 0.5 & \
./ConsoleInterface.r --bad-mouth --capability-set -r 0.5;
./ConsoleInterface.r --bad-mouth --service-set --capability-set -r 0.5 & \
./ConsoleInterface.r --bad-mouth --time-decay -r 0.5 & \
./ConsoleInterface.r --bad-mouth --service-set --capability-set --time-decay -r 0.5;
./ConsoleInterface.r --bad-mouth --service-set --time-decay -r 0.5 & \
./ConsoleInterface.r --bad-mouth --capability-set --time-decay -r 0.5;

# ./ConsoleInterface.r --good-mouth & \
# ./ConsoleInterface.r --good-mouth --service-set;
# ./ConsoleInterface.r --good-mouth --capability-set & \
# ./ConsoleInterface.r --good-mouth --service-set --capability-set & \
# ./ConsoleInterface.r --good-mouth --time-decay;
# ./ConsoleInterface.r --good-mouth --service-set --capability-set --time-decay & \
# ./ConsoleInterface.r --good-mouth --service-set --time-decay & \
# ./ConsoleInterface.r --good-mouth --capability-set --time-decay;
#
# ./ConsoleInterface.r --on-off & \
# ./ConsoleInterface.r --on-off --service-set & \
# ./ConsoleInterface.r --on-off --capability-set;
# ./ConsoleInterface.r --on-off --service-set --capability-set & \
# ./ConsoleInterface.r --on-off --time-decay & \
# ./ConsoleInterface.r --on-off --service-set --capability-set --time-decay;
# ./ConsoleInterface.r --on-off --service-set --time-decay & \
# ./ConsoleInterface.r --on-off --capability-set --time-decay;
