#!/bin/sh

./ConsoleInterface.r --bad_mouth --transactions=1000 & \
./ConsoleInterface.r --bad_mouth --capability_set --transactions=1000 & \
./ConsoleInterface.r --bad_mouth --time_decay --transactions=1000 & \
./ConsoleInterface.r --bad_mouth --capability_set --time_decay --transactions=1000
