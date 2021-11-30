#!/usr/bin/env zsh

set -eu

readonly R_DIR=R
readonly REPORT_DIR=report
readonly SCRIPT=$R_DIR/ods-benchmarking.R
readonly TEMPLATE=$R_DIR/ods-benchmarking.Rmd
readonly REPORT=$REPORT_DIR/ods-benchmarking

[[ ${1:-} == -f ]] &&
  Rscript -e "styler::style_file(c('$SCRIPT', '$TEMPLATE'), strict = T)"

Rscript $SCRIPT &&
  pandoc -f markdown -t html5 -s --self-contained --mathml < $REPORT.md > $REPORT.html
