#!/usr/bin/env zsh

set -e

# SRC=tutorial.nim
# SRC=salewski.nim
SRC=regex.nim

nim r $SRC $@
# nim --gc:arc r $SRC $@
