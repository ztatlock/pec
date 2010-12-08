#!/usr/bin/env bash

source $BOUNCER/script/common

function tester {
  bouncer.byte $1 > $BOUNCER/output/test-ast-$2 2>&1 
}

for t in $BOUNCER/test/ast/*.ast; do
  n=$(basename $t .ast)
  if tester $t $n; then
    printf "%-12s : %s\n" $n $PASS
  else
    printf "%-12s : %s\n" $n $FAIL
  fi
done

