#!/usr/bin/env bash

set -eu

readonly SOURCE=plot

Rscript "${SOURCE}.R" "${@}"
