#!/bin/sh

./ConsoleInterface.r --bad_mouth --transactions=500 --malicious_start=0.55 --malicious_end=0.55 --targeted && \
./ConsoleInterface.r --bad_mouth --capability_set --transactions=500 --malicious_start=0.55 --malicious_end=0.55 --targeted && \
./ConsoleInterface.r --bad_mouth --capability_set --transactions=500 --malicious_start=0.05 --malicious_end=0.05 --targeted && \
./ConsoleInterface.r --bad_mouth --capability_set --time_decay --transactions=500 --malicious_start=0.05 --malicious_end=0.05 --targeted && \
# ./ConsoleInterface.r --bad_mouth --transactions=500 --malicious_start=0.05 --malicious_end=0.05 --targeted && \
./ConsoleInterface.r --bad_mouth --capability_set --transactions=500 --malicious_start=0.30 --malicious_end=0.30 && \
./ConsoleInterface.r --bad_mouth --capability_set --time_decay --transactions=500 --malicious_start=0.30 --malicious_end=0.30
