#!/bin/bash

if [ $# -lt 1 ]; then
    echo "You need to specify a graph folder as arguments" \
        && exit
fi

FR_GRAPH_FOLDER=$1

for REP in -1 -0.5 0 0.25 0.5; do
    REP_FIGURES="$REP_FIGURES"$(source FillReputationFigures.sh "$FR_GRAPH_FOLDER" "$REP")
done

REP_FIGURES="$REP_FIGURES
slash_section{Nodemon Plots}
The following has plots of a monitored node throughout the life time of the
network at various reputation thresholds
"

for REP in -1 -0.5 0; do
    REP_FIGURES="$REP_FIGURES
    slash_subsection{Reputation $REP}
    $(source FillNodemon.sh "$FR_GRAPH_FOLDER" "$REP")"

done

REP_FIGURES="${REP_FIGURES//slash_/\\}"

REPORT=$(echo "$(cat AttackResultStart.tex)" "$REP_FIGURES" "$(cat AttackResultsEnd.tex)")
REPORT="${REPORT//slash_/\\}"

echo "$REPORT" > /tmp/report.tex
pdflatex /tmp/report.tex
pdflatex /tmp/report.tex
