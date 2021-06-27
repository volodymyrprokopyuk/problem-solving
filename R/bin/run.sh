#!/usr/bin/env bash

# readonly ROOT_DIR=$(pwd)

# readonly SOURCE=the-book-of-r
readonly SOURCE=advanced-r

Rscript "${SOURCE}.R" "${@}"
