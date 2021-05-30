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

# (echo "stdout"; echo "stderr" >&2) >/dev/null 2>&1
# (echo "stdout"; echo "stderr" >&2) &>/dev/null

# IFS="_ " read a b c d _ <<< "A_B_C D E_F"; echo "^$a, $b, $c, $d$"

# read l w c _ < <(wc reference.zsh); echo "$l $w $c"
# read l w c _ <<< $(wc reference.zsh); echo "$l $w $c"
# echo $(wc reference.zsh) | read l w c _; echo "$l $w $c"
