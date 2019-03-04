#!/bin/bash

if [ $# -lt 1 ]; then
    echo "You need to specify a graph folder, reputation folder, and attack folder as arguments" \
        && exit
fi

GRAPH_FOLDER=$1
REP_FOLDER=$2
ATTACK_FOLDER=$3

FIGURE_TEMPLATE=$(sed "s/graph_folder/$GRAPH_FOLDER/" AttackFigures.tex)
FIGURE_TEMPLATE="${FIGURE_TEMPLATE//rep_folder/$REP_FOLDER}"
FIGURE_TEMPLATE="${FIGURE_TEMPLATE//attack_folder/$ATTACK_FOLDER}"

FIGURES="${FIGURE_TEMPLATE//mal_num/5}"

for i in {10..95..5}; do
    FIGURES="$FIGURES
    ${FIGURE_TEMPLATE//mal_num/$i}"
done

printf  "%s" "$FIGURES"
