#!/bin/bash

# Uncomment to debug:
set -x

# Fail as soon as any program fails:
set -e

# TODO usage

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

# TODO stand-alone hcd-squash that reduces cycles like hcd-split-static

eval $STRIP | vcd-rename -r rename.txt | vcd-clock | vcd-transpose-hcd \
    | tee a.hcd |  hcd-io io-control.txt | tee io.hcd | hcd-output outputs.txt | hcd-force -f force-pins.txt \
    | tee all.hcd | hcd-split \
                >(hcd-expand-hus | hus-x1-hub | hub-binl pDynamic $BASE-pDynamic wX1 | $GZIP > $BASE-pDynamic.binl.gz) dynamic.txt \
                >(hcd-expand-hus | hus-x1-hub | hub-binl pStatic $BASE-pStatic wX1 | $GZIP > $BASE-pStatic.binl.gz) static.txt \
                >(tee jtag.hcd | hcd-expand-hus | hus-x1-hub | hub-binl pJtag $BASE-pJtag wX1 | $GZIP > $BASE-pJtag.binl.gz) jtag-pins.txt

# So vcd knows not to run again if nothing changed
md5 keep-signals.txt > keep-signals.txt.md5
ls -l $INP > $INP.ls

# Make multiport burst

make-burst $BASE-burst pDynamic $BASE-pDynamic pStatic $BASE-pStatic pJtag $BASE-pJtag > $BASE-burst

# TODO make pmf
