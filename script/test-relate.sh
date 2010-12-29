#!/usr/bin/env bash

source $BOUNCER/script/common

TEST="$BOUNCER/test/relate/*.rwr"
OUTP="$BOUNCER/output/relate"

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
  printf "%-30s " $nm
  if check $t $nm; then
    echo $PASS
  else
    echo $FAIL
  fi
done

