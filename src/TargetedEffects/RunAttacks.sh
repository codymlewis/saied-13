#!/bin/sh

./ConsoleInterface.r --bad-mouth -r  --tr 1000 & \
# ./ConsoleInterface.r --bad-mouth --service-set -r  --tr 1000 & \
./ConsoleInterface.r --bad-mouth --capability-set -r  --tr 1000 & \
# ./ConsoleInterface.r --bad-mouth --service-set --capability-set -r  --tr 1000;
./ConsoleInterface.r --bad-mouth --time-decay -r  --tr 1000 & \
# ./ConsoleInterface.r --bad-mouth --service-set --capability-set --time-decay -r  --tr 1000 & \
# ./ConsoleInterface.r --bad-mouth --service-set --time-decay -r  --tr 1000 & \
./ConsoleInterface.r --bad-mouth --capability-set --time-decay -r  --tr 1000;
