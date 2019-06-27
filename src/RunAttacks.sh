#!/bin/sh

./ConsoleInterface.r --bad_mouth --capability_set --total_nodes=200 --transactions=500 --malicious_start=0.35 --malicious_end=0.50 --targeted --type_calc=gn
./ConsoleInterface.r --bad_mouth --capability_set --total_nodes=200 --transactions=500 --malicious_start=0.25 --malicious_end=0.50 --targeted --type_calc=gcs
./ConsoleInterface.r --bad_mouth --capability_set --total_nodes=200 --transactions=500 --malicious_start=0.25 --malicious_end=0.50 --targeted --type_calc=gcns

# TODO: bigger networks with global mode, mc 55, other note altering strategies, with splitting vs without
