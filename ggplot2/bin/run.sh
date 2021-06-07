#!/usr/bin/env bash

readonly ROOT_DIR=$(pwd)

readonly SOURCE=plot

Rscript "${SOURCE}.r" "${@}"
