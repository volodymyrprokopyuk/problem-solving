#!/usr/bin/env zsh

# SRC=regex.nim
# SRC=json.nim
# SRC=options.nim
#
# SRC=tutorial.nim
# SRC=salewski.nim
SRC=nim_in_action.nim

# exec nim --verbosity:0 --threads:on --gc:orc r $SRC $@
exec nim --verbosity:0 --threads:on r $SRC $@
