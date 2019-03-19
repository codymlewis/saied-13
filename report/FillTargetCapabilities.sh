#!/bin/bash

FTC_GRAPH_FOLDER=$1

TC_TEMPLATE=$(sed "s/graph_folder/$FTC_GRAPH_FOLDER/g" TargetCapabilities.tex)

TC_TEXT=""

for attack in Bad_mouther Bad_mouther,_Capability_Setter Bad_mouther,_Capability_Setter,_Time_Decayer Bad_mouther,_Service_Setter Bad_mouther,_Service_Setter,_Capability_Setter Bad_mouther,_Service_Setter,_Capability_Setter,_Time_Decayer Bad_mouther,_Service_Setter,_Time_Decayer Bad_mouther,_Time_Decayer; do
    attack_type="${attack//_/ }"
    TC_ATTACK_TEMPLATE="${TC_TEMPLATE//attack_type/$attack_type}"
    TC_TEXT="$TC_TEXT
    ${TC_ATTACK_TEMPLATE//attack_folder/$attack}
    "
done

echo "$TC_TEXT"
