#!/usr/bin/env zsh

set -eu

case $1 in
  -c)
    shift
    nim r --threads:on client.nim $@
    ;;
  -s)
    shift
    nim r server.nim $@
    ;;
  *)
    echo "ERROR: unknown option $1"
    exit 1
    ;;
esac
