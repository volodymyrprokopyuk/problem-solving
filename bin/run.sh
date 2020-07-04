#!/usr/bin/env bash

readonly ROOT_DIR=$(pwd)

guile3.0 -L "${ROOT_DIR}" linear-algorithm.scm "${@}"
