#!/usr/bin/env zsh

# SRC=stdlib/regex.nim
# SRC=stdlib/json.nim
# SRC=stdlib/options.nim
# SRC=stdlib/threads.nim
# SRC=stdlib/locks.nim
SRC=stdlib/channels.nim

# SRC=tutorial.nim
# SRC=salewski.nim
# SRC=nim_in_action.nim
# SRC=chat/{client,server}.nim
# SRC=wiki/parser.nim

exec nim --verbosity:0 --threads:on r $SRC $@
