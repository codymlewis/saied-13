#!/bin/sh

./ConsoleInterface.r --bad_mouth --transactions=500 --malicious_start=0.55 --malicious_end=0.55 --targeted --type_calc=gn && \
./ConsoleInterface.r --bad_mouth --capability_set --transactions=500 --malicious_start=0.55 --malicious_end=0.55 --targeted --type_calc=gc
