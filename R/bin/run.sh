#!/usr/bin/env bash

set -eu

# readonly ROOT_DIR=$(pwd)

# readonly SOURCE=the-book-of-r
# readonly SOURCE=advanced-r
# readonly SOURCE=r-for-ds

# Rscript $SOURCE.R ${@}


readonly SOURCE=ipf-benchmarking

[[ ${1:-} == -s ]] &&
  Rscript -e "styler::style_file(c('$SOURCE.R', '$SOURCE.Rmd'), strict = T)"

Rscript $SOURCE.R &&
  pandoc -f markdown -t html5 -s --self-contained --mathml < $SOURCE.md > $SOURCE.html
