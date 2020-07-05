#!/usr/bin/env bash

readonly ROOT_DIR=$(pwd)

# readonly SOURCE=linear-algorithm
readonly SOURCE=conditional-algorithm

# guile3.0 -L "${ROOT_DIR}" "${SOURCE}".scm "${@}"
guile3.0 -L "${ROOT_DIR}" "${SOURCE}"-test.scm "${@}"
