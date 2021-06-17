#!/usr/bin/env zsh

set -eu

readonly DOC=minimal
# readonly DOC=cts-benchmarking-2021-06-08
# readonly STYLE=style/base.min.css
# readonly STYLE=style/tailwind.min.css

function cleanup {
  rm -rf figure $DOC.{md,html,pdf}
}

function rmd_to_md {
  Rscript -e "library(knitr); library(ggplot2); knit('$1.Rmd')"
}

function md_to_html {
  pandoc -f markdown -t html5 -s --self-contained --mathml --toc # -c $STYLE
}

function md_to_pdf {
  pandoc -f markdown -t pdf --pdf-engine wkhtmltopdf -s --mathml # -c $STYLE
}

# cleanup

rmd_to_md $DOC && md_to_html < $DOC.md > $DOC.html
# rmd_to_md $DOC && md_to_pdf < $DOC.md > $DOC.pdf

# rmd_to_md $DOC && md_to_html < $DOC.md > $DOC.html & md_to_pdf < $DOC.md > $DOC.pdf
