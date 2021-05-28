#!/usr/bin/env zsh

set -eu

function map_reduce {
  mkfifo p{i,o}{1,2,3}
  function cleanup { rm -f p{i,o}{1,2,3} }
  trap cleanup INT TERM EXIT

  tr a A < pi1 > po1 &
  sed 's/l/L/' < pi2 > po2 &
  cut -c 2- < pi3 > po3 &

  tee pi{1,2} > pi3 &
  cat po{1,2,3}
}

# printf '%s\n' vlad lana | map_reduce

# read a b c <<< "A B C"; echo "$a $b $c"

(echo "stdout"; echo "stderr" >&2) >/dev/null 2>&1
(echo "stdout"; echo "stderr" >&2) &>/dev/null
