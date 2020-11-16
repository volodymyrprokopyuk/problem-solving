#!/usr/bin/env bash

readonly ROOT_DIR=$(pwd)
readonly SRFI_DIR=$HOME/Projects/scheme-upgrade

# readonly SOURCE=playground-algorithm
# readonly SOURCE=shell-algorithm
# readonly SOURCE=comparator-algorithm
readonly SOURCE=iteration-algorithm
# readonly SOURCE=continuation-algorithm

export GAUCHE_LOAD_PATH="${ROOT_DIR}:${SRFI_DIR}"

gosh "${SOURCE}".scm "${@}"