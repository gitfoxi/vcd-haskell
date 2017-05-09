#!/bin/bash

set -x
set -e

PORT=NO_REF_CLK

INP=$1
BASE=`basename $1 .vcd`
ROOT=`stack path | egrep "^local-install-root:" | sed "s/^local-install-root: *//"`
BIN=$ROOT/bin
PATH=$BIN:$PATH

STRIPPED=$BASE-stripped.vcd

if [ ! -e $STRIPPED ]
then
    v-strip keep-signals.txt $INP > $STRIPPED
fi

cat $STRIPPED | v-rename rename.txt | v-clock | vcd-transpose-hcd | hcd-expand-hus | x1 $BASE.hus | hub-binl $PORT $BASE wX1 | pigz > $BASE.binl.gz
