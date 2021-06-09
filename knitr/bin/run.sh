#!/usr/bin/env bash

set -eu

readonly SOURCE=minimal

rm -rf figure $SOURCE.md
Rscript -e "library(knitr); knit(\"$SOURCE.Rmd\")"
