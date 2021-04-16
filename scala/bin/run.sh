#!/usr/bin/env bash

readonly ROOT_DIR=$(pwd)

readonly SOURCE=playground-algorithm

scala -deprecation "${SOURCE}.scala" "${@}"
