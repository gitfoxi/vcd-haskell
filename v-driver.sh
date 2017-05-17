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
$GZIP --version 2>&1 || { echo "Warning: pigz not installed - will use gzip instead which is slower"; GZIP=gzip; }

STRIPPED=$BASE-stripped.vcd

# Only run vcd-strip if it seems the INP file or keep-signals.txt changed
if [ ! -e $STRIPPED ] \
       || [ ! -e keep-signals.txt.md5 ] \
       || [ ! -e $INP.ls ] \
       || ! diff -q keep-signals.txt.md5 <(md5 keep-signals.txt) \
       || ! diff -q $INP.ls <(ls -l $INP)
then
    echo Running vcd-strip because of new VCD or keep-signals.txt
    STRIP="vcd-strip -k keep-signals.txt $INP | tee $STRIPPED"
else
    STRIP="cat $STRIPPED"
fi

eval $STRIP | vcd-rename -r rename.txt | vcd-clock | vcd-transpose-hcd | hcd-expand-hus | hus-x1-hub | hub-binl $PORT $BASE wX1 | $GZIP > $BASE.binl.gz

# So vcd knows not to run again if nothing changed
md5 keep-signals.txt > keep-signals.txt.md5
ls -l $INP > $INP.ls

# remove TCK TODO hcd-rm -- just use hcd-split
cat $STRIPPED | vcd-rename -r rename.txt | vcd-clock | vcd-transpose-hcd | sed -e '/^jtg_tck/ { N; N; d; }' > $BASE.hcd

hcd-split-static -s $BASE-static.hcd -d $BASE-dynamic.hcd -S $BASE-static-pins.txt -D $BASE-dynamic-pins.txt < $BASE.hcd

# cat $BASE-static.hcd | hcd-expand-hus

# io, output when control sig = 1
