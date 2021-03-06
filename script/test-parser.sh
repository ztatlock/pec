#!/usr/bin/env bash

source $PEC/script/common

TEST="$PEC/test/parser/*.rwr"
OUTP="$PEC/output/parser"

function run {
  ckparse $1 > $OUTP-$2 2>&1
}

function check {
  expect=$(echo $2 | sed 's/^\(.\).*/\1/')
  if run $1 $2; then
    [ "$expect" = "p" ]
  else
    [ "$expect" = "n" ]
  fi
}

for t in $TEST; do
  nm=$(basename $t .rwr)
  printf "%-20s " $nm
  if check $t $nm; then
    echo $PASS
  else
    echo $FAIL
  fi
done

