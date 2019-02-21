#!/bin/bash

FN_GRAPH_FOLDER=$1
FN_REP=$2

NODEMON_TEMPLATE=$(sed "s/graph_folder/$FN_GRAPH_FOLDER/g" Nodemon.tex | sed "s/rep_folder/$FN_REP/g")

NODEMON_TEXT=""

for attack in Bad_mouther Bad_mouther,_Capability_Setter Bad_mouther,_Capability_Setter,_Time_Decayer Bad_mouther,_Capability_Setter,_Time_Decayer Bad_mouther,_Service_Setter Bad_mouther,_Service_Setter,_Capability_Setter Bad_mouther,_Service_Setter,_Capability_Setter,_Time_Decayer Bad_mouther,_Service_Setter,_Time_Decayer Bad_mouther,_Service_Setter,_Time_Decayer Bad_mouther,_Time_Decayer; do
    NODEMON_TEXT="$NODEMON_TEXT
    ${NODEMON_TEMPLATE//attack_folder/$attack}
    "
done

echo "$NODEMON_TEXT"
