#!/bin/sh

./ConsoleInterface.r --bad_mouth --capability_set --total_nodes=250 --transactions=500 --malicious_start=0.55 --malicious_end=0.55 --targeted --type_calc=gcns && \
./ConsoleInterface.r --bad_mouth --capability_set --total_nodes=200 --transactions=500 --malicious_start=0.55 --malicious_end=0.55 --targeted --type_calc=gs && \
./ConsoleInterface.r --bad_mouth --capability_set --total_nodes=200 --transactions=500 --malicious_start=0.55 --malicious_end=0.55 --targeted --type_calc=mca
