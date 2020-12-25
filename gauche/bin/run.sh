#!/usr/bin/env bash

readonly ROOT_DIR=$(pwd)
readonly SRFI_DIR=$HOME/Projects/scheme-upgrade

# readonly SOURCE=playground-algorithm
# readonly SOURCE=shell-algorithm
# readonly SOURCE=comparator-algorithm
# readonly SOURCE=continuation-algorithm
# readonly SOURCE=iterative-algorithm
# readonly SOURCE=conditional-algorithm
readonly SOURCE=recursive-algorithm
# readonly SOURCE=tic-tac-toe
# readonly SOURCE=recambios

export GAUCHE_LOAD_PATH="${ROOT_DIR}:${SRFI_DIR}"

gosh "${SOURCE}.scm" "${@}"
# gosh -m tic-tac-toe "${SOURCE}.scm" "${@}"
# gosh -m recambios "${SOURCE}.scm" "${@}"
