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
# readonly SOURCE=string-algorithm
# readonly SOURCE=file-algorithm
# readonly SOURCE=time-algorithm
# readonly SOURCE=control-algorithm
# readonly SOURCE=fsm-algorithm
# readonly SOURCE=abstraction-algorithm
# readonly SOURCE=backtracking-algorithm
# readonly SOURCE=set-algorithm
# readonly SOURCE=vector-algorithm
readonly SOURCE=sorting-algorithm

export GUILE_LOAD_PATH="${ROOT_DIR}:${SRFI_DIR}"

guile3.0 "${SOURCE}".scm "${@}"

# for test_file in iter*-test.scm; do
#     guile3.0 "${test_file}"
# done
