#!/usr/bin/env bash

source $PEC/script/common

TEST="$PEC/test/relate/*.rwr"
OUT="$PEC/output"
OUTP="$OUT/relate"

function run {
  pec $1 -s $OUT > $OUTP-$2 2>&1
  mv $OUT/pec-log $OUTP-$2-log
  tail -n 1 $OUTP-$2
}

function check {
  expect=$(echo $2 | sed 's/^\(.\).*/\1/')
  actual=$(run $1 $2)
  ( [ "$expect" = "p" ] && [ "$actual" = "VALID"   ] ) || \
  ( [ "$expect" = "n" ] && [ "$actual" = "INVALID" ] )
}

function nQueries {
  grep '>>> Query #' $OUTP-$1-log \
    | tail -n 1 \
    | awk '{ print $NF }'
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
  lns=$(wc -l $t | awk '{ print $1 }')
  sec=$(expr $tm2 - $tm1)
  nqs=$(nQueries $nm)
  printf "%-30s %s  %2d  %2d  %2d\n" $nm $res $lns $sec $nqs
done

