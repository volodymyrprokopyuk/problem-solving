#!/usr/bin/env bash

readonly ROOT_DIR=$(pwd)
readonly SRFI_DIR=$HOME/Projects/scheme-upgrade

# readonly SOURCE=linear-algorithm
# readonly SOURCE=conditional-algorithm
# readonly SOURCE=recursive-algorithm
# readonly SOURCE=iterative-algorithm
# readonly SOURCE=array-algorithm
# readonly SOURCE=data-structure
# readonly SOURCE=stack-algorithm
# readonly SOURCE=queue-algorithm
readonly SOURCE=string-algorithm

export GUILE_LOAD_PATH="${ROOT_DIR}:${SRFI_DIR}"

# guile3.0 "${SOURCE}".scm "${@}"

for test_file in str*-test.scm; do
    guile3.0 "${test_file}"
done
