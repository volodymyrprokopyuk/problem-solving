#!/usr/bin/env bash

set -eu

readonly SOURCE=plot

exec Rscript "${SOURCE}.R"
