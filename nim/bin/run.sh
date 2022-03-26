#!/usr/bin/env zsh

# SRC=stdlib/json.nim
# SRC=stdlib/options.nim
# SRC=stdlib/threads.nim
# SRC=stdlib/locks.nim
# SRC=stdlib/channels.nim
# SRC=stdlib/strformat.nim

# SRC=pkg/regex.nim
SRC=pkg/nimja.nim

# SRC=tutorial.nim
# SRC=salewski.nim
# SRC=nim_in_action.nim
# SRC=chat/{client,server}.nim
# SRC=wiki/parser.nim

exec nim --verbosity:0 --threads:on r $SRC $@
