#!/usr/bin/env bash

readonly ROOT_DIR=$(pwd)

# readonly SOURCE=playground-algorithm
# readonly SOURCE=shell-algorithm
# readonly SOURCE=comparator-algorithm
# readonly SOURCE=continuation-algorithm
# readonly SOURCE=iterative-algorithm
# readonly SOURCE=conditional-algorithm
# readonly SOURCE=recursive-algorithm
# readonly SOURCE=tree-algorithm
# readonly SOURCE=syntax-algorithm
# readonly SOURCE=lazy-algorithm
# readonly SOURCE=dstructure
readonly SOURCE=impatient

gosh "${SOURCE}.scm" "${@}"

# readonly SOURCE=tic-tac-toe
# readonly SOURCE=recambios
# readonly SOURCE=spell-number
# readonly SOURCE=pattern-matcher

# gosh -m tic-tac-toe "${SOURCE}.scm" "${@}"
# gosh -m recambios "${SOURCE}.scm" "${@}"
# gosh -m spell-number "${SOURCE}.scm" "${@}"
# gosh -m pattern-matcher "${SOURCE}.scm" "${@}"
