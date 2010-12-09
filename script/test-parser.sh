#!/usr/bin/env bash

source $BOUNCER/script/common

function tester {
  checkParser $1 > $BOUNCER/output/test-parser-$2 2>&1 
}

for t in $BOUNCER/test/parser/*.rwr; do
  n=$(basename $t .rwr)
  if tester $t $n; then
    printf "%-12s : %s\n" $n $PASS
  else
    printf "%-12s : %s\n" $n $FAIL
  fi
done

