#!/usr/bin/env bash

readonly ROOT_DIR=$(pwd)

# readonly SOURCE=linear-algorithm
# readonly SOURCE=conditional-algorithm
# readonly SOURCE=recursive-algorithm
readonly SOURCE=iterative-algorithm

# guile3.0 -L "${ROOT_DIR}" "${SOURCE}".scm "${@}"

for test_file in iter*-test.scm; do
    guile3.0 -L "${ROOT_DIR}" "${test_file}"
done
