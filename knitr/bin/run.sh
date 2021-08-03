#!/usr/bin/env zsh

set -eu

readonly LIB="library(knitr); library(readr); library(dplyr); library(ggplot2);
library(gt); library(gcookbook)"

# readonly DOC=minimal
readonly DOC=presupuesto-cubierta
# readonly DOC=presupuesto-techos-paredes
# readonly DOC=cts-benchmarking-2021-06-08


function cleanup {
  rm -rf figure $DOC.{md,html,pdf}
}

function rmd_to_md {
  Rscript -e "$LIB; knit('$1.Rmd')"
}

function md_to_html {
  pandoc -f markdown -t html5 -s --self-contained --mathml
}

function md_to_pdf {
  pandoc -f markdown -t pdf -s --pdf-engine wkhtmltopdf --mathml
}

# cleanup

rmd_to_md $DOC && md_to_html < $DOC.md > $DOC.html
# rmd_to_md $DOC && md_to_pdf < $DOC.md > $DOC.pdf

# rmd_to_md $DOC && md_to_html < $DOC.md > $DOC.html & md_to_pdf < $DOC.md > $DOC.pdf
