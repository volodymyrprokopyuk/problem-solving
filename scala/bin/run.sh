#!/usr/bin/env bash

readonly ROOT_DIR=$(pwd)

readonly SOURCE=impatient

scala -i 'hr.scala' -deprecation "${SOURCE}.scala" "${@}"
