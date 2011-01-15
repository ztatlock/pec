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

echo "Test                           Check Lns Sec ATP"
echo "------------------------------------------------"

for t in $TEST; do
  tnm=$(basename $t .rwr)
  tm1=$(date +%s)
  res=$(check $t $tnm && echo $PASS || echo $FAIL)
  tm2=$(date +%s)
  lns=$(wc -l $t | awk '{ print $1 }')
  sec=$(expr $tm2 - $tm1)
  nqs=$(nQueries $tnm)
  printf "%-30s %16s %3d %3d %3d\n" $tnm $res $lns $sec $nqs
done

