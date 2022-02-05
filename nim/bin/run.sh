#!/usr/bin/env zsh

set -e

# SRC=tutorial.nim
# SRC=salewski.nim
# SRC=regex.nim
SRC=nim_in_action.nim

nim --verbosity:0 r $SRC $@
# nim --gc:arc r $SRC $@
