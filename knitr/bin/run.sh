#!/usr/bin/env zsh

set -eu

readonly LIB="library(knitr); library(ggplot2)"

readonly DOC=minimal
# readonly DOC=mflow
# readonly DOC=presupuesto-cubierta
# readonly DOC=presupuesto-techos-paredes
# readonly DOC=cts-benchmarking-2021-06-08

function format {
  Rscript -e "styler::style_file('$DOC.Rmd', strict = T)"
}

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

[[ ${1:-} == -f ]] && format

# cleanup

rmd_to_md $DOC && md_to_html < $DOC.md > $DOC.html
# rmd_to_md $DOC && md_to_pdf < $DOC.md > $DOC.pdf

# rmd_to_md $DOC && md_to_html < $DOC.md > $DOC.html & md_to_pdf < $DOC.md > $DOC.pdf
