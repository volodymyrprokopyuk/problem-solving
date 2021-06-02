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

# a="vlad and lana"
# echo ${a//a/\*}
# echo ${#a}

# exec 3< reference.zsh
# grep 'function' <&3
# exec 3>&-
# exec 3< reference.zsh
# read l <&3; echo $l

tx_data="TX001;1.23;EUR;Buy milk
TX002;123;EUR;Pay the rent
TX003;12.3;EUR;Order one book"

# (extract DSL | format DSL) < tx_data

(sed -E 's/^([^;]+);([^;]+);([^;]+);([^;]+)$/\1 \2 \3 \4/' \
   | while read id amount currency subject
 do printf "%-7s %8.2f %-4s %15s\n" $id $amount $currency $subject; done) <<< "$tx_data"

# (extract_transaction | format_transaction) < tx_data

function extract_transaction {
  while read transaction
  do echo "$transaction"
  done | while IFS=';' read id amount currency subject
  do echo "$id $amount $currency $subject"
  done
}

function format_transaction {
  while read id amount currency subject
  do
    printf -v pad "%15s"
    local fid="$id${pad:0:$((7 - ${#id}))}"
    printf -v famount "%8.2f" $amount
    local fcurrency="$currency${pad:0:$((4 - ${#currency}))}"
    local fsubject="${pad:0:$((15 - ${#subject}))}$subject"
    echo "$fid $famount $fcurrency $fsubject"
  done
}

(extract_transaction | format_transaction) <<< "$tx_data"
