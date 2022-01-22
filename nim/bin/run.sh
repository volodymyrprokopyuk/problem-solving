#!/usr/bin/env zsh

set -e

# SRC=tutorial.nim
SRC=salewski.nim

[[ $1 == -f ]] && nimpretty $SRC
nim r $SRC
