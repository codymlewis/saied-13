#!/bin/sh

./ConsoleInterface.r --bad_mouth --capability_set --total_nodes=300 --transactions=500 --malicious_start=0.55 --malicious_end=0.55 --targeted --type_calc=gcns && \
./ConsoleInterface.r --bad_mouth --capability_set --total_nodes=400 --transactions=500 --malicious_start=0.55 --malicious_end=0.55 --targeted --type_calc=gcns && \
./ConsoleInterface.r --bad_mouth --capability_set --total_nodes=200 --transactions=500 --malicious_start=0.55 --malicious_end=0.55 --targeted --type_calc=gcn && \
./ConsoleInterface.r --bad_mouth --capability_set --total_nodes=200 --transactions=500 --malicious_start=0.55 --malicious_end=0.55 --targeted --type_calc=gcnsa
