#!/usr/bin/env bash

source $PEC/script/common

TEST="$PEC/test/relate/*.rwr"
OUTP="$PEC/output/relate"

function run {
  pec $1 \
   1> $OUTP-$2 \
   2> $OUTP-$2-error
  tail -n 1 $OUTP-$2
}

function check {
  expect=$(echo $2 | sed 's/^\(.\).*/\1/')
  actual=$(run $1 $2)
  ( [ "$expect" = "p" ] && [ "$actual" = "VALID"   ] ) || \
  ( [ "$expect" = "n" ] && [ "$actual" = "INVALID" ] )
}

for t in $TEST; do
  nm=$(basename $t .rwr)
  tm1=$(date +%s)
  if check $t $nm; then
    res=$PASS
  else
    res=$FAIL
  fi
  tm2=$(date +%s)
  sec=$(expr $tm2 - $tm1)
  printf "%-30s %s  %d\n" $nm $res $sec
done

