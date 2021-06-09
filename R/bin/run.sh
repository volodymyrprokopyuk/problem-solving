#!/usr/bin/env bash

# readonly ROOT_DIR=$(pwd)

readonly SOURCE=playground

Rscript "${SOURCE}.R" "${@}"
