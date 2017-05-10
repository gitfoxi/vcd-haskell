#!/bin/bash

# Uncomment to debug:
set -x

# Fail as soon as any program fails:
set -e

PORT=NO_REF_CLK

INP=$1
BASE=`basename $1 .vcd`
ROOT=`stack path | egrep "^local-install-root:" | sed "s/^local-install-root: *//"`
BIN=$ROOT/bin
PATH=$BIN:$PATH

GZIP=pigz
$GZIP -v 2>&1 || { echo "Warning: pigz not installed - will use gzip instead which is slower"; GZIP=gzip; }

STRIPPED=$BASE-stripped.vcd

if [ ! -e $STRIPPED ]
then
    v-strip keep-signals.txt $INP > $STRIPPED
fi

cat $STRIPPED | v-rename rename.txt | v-clock | vcd-transpose-hcd | hcd-expand-hus | x1 | hub-binl $PORT $BASE wX1 | $GZIP > $BASE.binl.gz

# remove TCK TODO hcd-rm
cat $STRIPPED | v-rename rename.txt | v-clock | vcd-transpose-hcd | sed -e '/^jtg_tck/ { N; N; d; }' > $BASE.hcd

hcd-split-static $BASE-static.hcd $BASE-dynamic.hcd $BASE-static-pins.txt < $BASE.hcd

cat $BASE-static.hcd | hcd-expand-hus

# io, output when control sig = 1
