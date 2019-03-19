#!/bin/bash

if [ $# -lt 1 ]; then
    echo "You need to specify a graph folder as arguments" \
        && exit
fi

GTE_GRAPH_FOLDER=$1

REPORT="slash_documentclass{report}
slash_usepackage{graphicx}
slash_usepackage[margin=0.85in]{geometry}
slash_usepackage[english]{babel}
slash_usepackage{float}
slash_usepackage{amsmath}
slash_usepackage{url}
slash_usepackage{cite}
slash_usepackage{amssymb}
slash_usepackage{booktabs}
slash_usepackage{tabularx}
slash_usepackage{hyperref}

slash_title{Trust Model Attack Results}
slash_author{Cody Lewis}
slash_date{slash_today}

slash_begin{document}
  slash_maketitle
    slash_section{Effects on a Target Group}
    The following plots show the effects on the trust of the attacks on a targeted
    group who have their service and capability in the range $ [45, 55] $ versus
    a normal group who do not fall in that range.
    $(source FillTargetCapabilities.sh "$GTE_GRAPH_FOLDER")
slash_end{document}"

REPORT="${REPORT//slash_/\\}"

echo "$REPORT" > /tmp/target_effects_report.tex
pdflatex /tmp/target_effects_report.tex
pdflatex /tmp/target_effects_report.tex
