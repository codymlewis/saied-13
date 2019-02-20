#!/bin/bash

if [ $# -lt 1 ]; then
    echo "You need to specify a graph folder, and reputation folder as arguments" \
        && exit
fi

FRF_GRAPH_FOLDER=$1
FRF_REP_FOLDER=$2

ATTACK_FIGURES=$(source FillAttackFigures.sh "$FRF_GRAPH_FOLDER" "$FRF_REP_FOLDER" Bad_mouther)
REPUTATION_FIGURES=$(perl  -pe "s~^bm$~$ATTACK_FIGURES~" < ReputationFigures.tex)

ATTACK_FIGURES=$(source FillAttackFigures.sh "$FRF_GRAPH_FOLDER" "$FRF_REP_FOLDER" Bad_mouther,_Capability_Setter)
REPUTATION_FIGURES=$(echo "$REPUTATION_FIGURES" | perl  -pe "s~^cs$~$ATTACK_FIGURES~")

ATTACK_FIGURES=$(source FillAttackFigures.sh "$FRF_GRAPH_FOLDER" "$FRF_REP_FOLDER" Bad_mouther,_Capability_Setter,_Time_Decayer)
REPUTATION_FIGURES=$(echo "$REPUTATION_FIGURES" | perl  -pe "s~^cstd$~$ATTACK_FIGURES~")

ATTACK_FIGURES=$(source FillAttackFigures.sh "$FRF_GRAPH_FOLDER" "$FRF_REP_FOLDER" Bad_mouther,_Service_Setter)
REPUTATION_FIGURES=$(echo "$REPUTATION_FIGURES" | perl  -pe "s~^ss$~$ATTACK_FIGURES~")

ATTACK_FIGURES=$(source FillAttackFigures.sh "$FRF_GRAPH_FOLDER" "$FRF_REP_FOLDER" Bad_mouther,_Service_Setter,_Capability_Setter)
REPUTATION_FIGURES=$(echo "$REPUTATION_FIGURES" | perl  -pe "s~^sscs$~$ATTACK_FIGURES~")

ATTACK_FIGURES=$(source FillAttackFigures.sh "$FRF_GRAPH_FOLDER" "$FRF_REP_FOLDER" Bad_mouther,_Service_Setter,_Capability_Setter,_Time_Decayer)
REPUTATION_FIGURES=$(echo "$REPUTATION_FIGURES" | perl  -pe "s~^sscstd$~$ATTACK_FIGURES~")

ATTACK_FIGURES=$(source FillAttackFigures.sh "$FRF_GRAPH_FOLDER" "$FRF_REP_FOLDER" Bad_mouther,_Service_Setter,_Time_Decayer)
REPUTATION_FIGURES=$(echo "$REPUTATION_FIGURES" | perl  -pe "s~^sstd$~$ATTACK_FIGURES~")

ATTACK_FIGURES=$(source FillAttackFigures.sh "$FRF_GRAPH_FOLDER" "$FRF_REP_FOLDER" Bad_mouther,_Time_Decayer)
REPUTATION_FIGURES=$(echo "$REPUTATION_FIGURES" | perl  -pe "s~^td$~$ATTACK_FIGURES~")

printf "slash_section*{Reputation $FRF_REP_FOLDER}\n%s\n" "$REPUTATION_FIGURES"
