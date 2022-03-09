#!/usr/bin/env zsh

# SRC=regex.nim
# SRC=json.nim
# SRC=options.nim
# SRC=threads.nim

# SRC=tutorial.nim
# SRC=salewski.nim
SRC=nim_in_action.nim
# SRC=chat/{client,server}.nim
# SRC=wiki/parser.nim

exec nim --verbosity:0 --threads:on r $SRC $@
