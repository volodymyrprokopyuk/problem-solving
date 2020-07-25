#!/usr/bin/env bash

readonly ROOT_DIR=$(pwd)
readonly SRFI_DIR=$HOME/Projects/scheme-upgrade

# readonly SOURCE=linear-algorithm
# readonly SOURCE=conditional-algorithm
readonly SOURCE=recursive-algorithm
# readonly SOURCE=iterative-algorithm
# readonly SOURCE=array-algorithm

export GUILE_LOAD_PATH="${ROOT_DIR}:${SRFI_DIR}"

# guile3.0 "${SOURCE}".scm "${@}"

for test_file in rec*-test.scm; do
    guile3.0 "${test_file}"
done
