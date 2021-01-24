#!/usr/bin/env bash

set -eu

readonly ROOT_DIR=$(pwd)

readonly DATA=$ROOT_DIR/bookstore.xml
readonly QUERY=$ROOT_DIR/query.xql

basex -i "${DATA}" "${QUERY}"
