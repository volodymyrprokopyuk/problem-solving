#!/usr/bin/env zsh

set -eu

readonly DOC=minimal
# readonly DOC=cts-benchmarking-2021-06-08
# readonly STYLE=style/modern-normalize.min.css
# readonly STYLE=style/base.min.css
# readonly STYLE=style/tailwind.min.css
readonly STYLE=style/style.css

function cleanup {
  rm -rf figure $DOC.{md,html,pdf}
}

function rmd_to_md {
  command r -e "library(knitr); library(ggplot2); knit('$1.Rmd')"
}

function md_to_html {
  pandoc -f markdown -t html5 -s --self-contained --mathml -c $STYLE
}

function md_to_pdf {
  pandoc -f markdown -t pdf -s --pdf-engine wkhtmltopdf --mathml -c $STYLE
}

# cleanup

rmd_to_md $DOC && md_to_html < $DOC.md > $DOC.html
# rmd_to_md $DOC && md_to_pdf < $DOC.md > $DOC.pdf

# rmd_to_md $DOC && md_to_html < $DOC.md > $DOC.html & md_to_pdf < $DOC.md > $DOC.pdf
