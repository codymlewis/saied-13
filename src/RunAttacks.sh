#!/bin/sh

mal_start=0.50
# mal_start=0.55
mal_end=$mal_start

./ConsoleInterface.r --bad_mouth --capability_set --total_nodes=200 --transactions=500 --malicious_start=$mal_start --malicious_end=$mal_end --targeted --type_calc=gcns &
./ConsoleInterface.r --bad_mouth --capability_set --total_nodes=200 --transactions=500 --malicious_start=$mal_start --malicious_end=$mal_end --targeted --type_calc=gcn &
./ConsoleInterface.r --bad_mouth --capability_set --total_nodes=200 --transactions=500 --malicious_start=$mal_start --malicious_end=$mal_end --targeted --type_calc=gns &
./ConsoleInterface.r --bad_mouth --total_nodes=200 --transactions=500 --malicious_start=$mal_start --malicious_end=$mal_end --targeted --type_calc=gns
