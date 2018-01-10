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

OUT=$BASE
CP_TO=caops1:o78/device/inbox/$BASE

mkdir -p $OUT

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

# XXX sync.pipe is the current workaround for the problem that bash doesn't
# wait for process substitutions to finish. Maybe some parallelism is not
# worth the bugs
# http://stackoverflow.com/questions/4489139/bash-process-substitution-and-syncing
rm -f sync.pipe
mkfifo sync.pipe

eval $STRIP | vcd-rename -r rename.txt | vcd-clock | vcd-transpose-hcd \
    | tee a.hcd |  hcd-io io-control.txt | tee io.hcd | hcd-output outputs.txt | hcd-force -f force-pins.txt \
    | tee all.hcd | hcd-split \
                >(hcd-expand-hus | hus-x1-hub | hub-binl -p pDynamic -l $BASE-pDynamic -w wX1 | $GZIP > $OUT/$BASE-pDynamic.binl.gz; echo done >sync.pipe) dynamic.txt \
                >(hcd-expand-hus | hus-x1-hub | hub-binl -p pStatic -l $BASE-pStatic -w wX1 | $GZIP > $OUT/$BASE-pStatic.binl.gz; echo done >sync.pipe) static.txt \
                >(tee jtag.hcd | hcd-expand-hus | tee jtag.hus | hus-x1-hub > jtag.hub; echo done >sync.pipe) jtag-pins.txt

read line < sync.pipe
read line < sync.pipe
read line < sync.pipe

ls -l $OUT

hus-jtag-comments jtag.hus > jtag-dr.comments
COMMENTS=jtag-dr.comments

# Look for device-specific commenter
if [ -e ./jtag-expand-comments ]
then
    ./jtag-expand-comments < $COMMENTS > device.comments
    COMMENTS=device.comments
fi

hub-binl jtag.hub -c $COMMENTS -p pJtag -l $BASE-pJtag -w wX1 | $GZIP > $OUT/$BASE-pJtag.binl.gz

# So vcd knows not to run again if nothing changed
md5 keep-signals.txt > keep-signals.txt.md5
ls -l $INP > $INP.ls

# Make multiport burst

make-burst $BASE-burst pDynamic $BASE-pDynamic pStatic $BASE-pStatic pJtag $BASE-pJtag > $OUT/$BASE-burst

# Make DFPT to copy into config
./make-dfpt.py pDynamic < dynamic.txt > $OUT/$BASE.dfpt
./make-dfpt.py pStatic < static.txt >> $OUT/$BASE.dfpt
./make-dfpt.py pJtag < jtag-pins.txt >> $OUT/$BASE.dfpt

# TODO make pmf, DFPT

# Copy to tester
rsync -av $OUT/ $CP_TO

# Clean up
rm -f sync.pipe
