#!/bin/bash

# Uncomment to debug:
# set -x

# Fail as soon as any program fails:
set -e

INP=$1
KEEP=$2
TCK=$3
PORT=$4
LABEL=$5
WAVETABLE=$6

if [ "$#" -ne 6 ]; then
    echo "Error: needs 6 arguments

Usage: simple-x1.sh INPUT.vcd KEEP-WIRES.txt TCK-PIN PORT LABEL WAVETABLE-NAME

Example: simple-x1.sh jtag.vcd keep-signals.txt jtg_tck pJtag jtag-test X1

The example creates a 93k SmartScale vector in jtag-test.binl. The vector
targets the port pJtag. You should provide an X1 wavetable that looks like:

PINS DATA_PINS
0 \"d1:0\"
1 \"d1:1\"
2 \"d1:Z r1:X\"
3 \"d1:Z r1:L\"
4 \"d1:Z r1:H\"
brk \"\"

PINS jtg_tck CLOCKS
0 \"d1:0 d2:1\"
brk \"d1:0 d2:1\"
"
    exit 1
fi

vcd-strip $KEEP $INP | \
vcd-clock $TCK | \
vcd-transpose-hcd | \
hcd-expand-hus | \
hus-x1-hub | \
hub-binl $PORT $LABEL $WAVETABLE > $LABEL.binl
