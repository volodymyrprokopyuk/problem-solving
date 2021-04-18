#!/usr/bin/env bash

readonly ROOT_DIR=$(pwd)

readonly SOURCE=impatient

scala -deprecation "${SOURCE}.scala" "${@}"
