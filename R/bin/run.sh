#!/usr/bin/env bash

# readonly ROOT_DIR=$(pwd)

# readonly SOURCE=the-book-of-r
# readonly SOURCE=advanced-r
readonly SOURCE=r-for-ds
# readonly SOURCE=ipf-benchmarking

Rscript "${SOURCE}.R" "${@}"
