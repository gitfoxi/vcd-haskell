
vcd-haskell
==============

Convert VCD to SmartScale 93k test vectors.

Features
----------

* Fast
* Flexible
* Clock-based sampling
* X-mode compression
* Standard X1 and X2 wavetables
* Custom wavetables for any X mode
* Repeat compression
* VECC (ALDC) compression
* Binary (BINL) vector output
* Multiport
* Add, remove, force, mask and rename signals


TODO
-----

* Multiport burst
* Time-based sampling
* Non-standard waveforms
* JTAG state comments

Install
---------

1. Install [stack](https://docs.haskellstack.org/en/stable/README/)
2. Use stack to build and install

    stack build
    stack install

3. Create a symlink to libaldc (only if you want VECC compression) - TODO

Quick Start
--------------

    vcd-strip keep-signals.txt jtag.vcd | \
    vcd-clock jtg_tck | \
    vcd-transpose-hcd | \
    hcd-expand-hus | \
    hus-x1-hub | \
    hub-binl portJtag jtag-test wavetableX1 > \
    jtag-test.binl

My approach is to provide a collection of small, single-purpose programs that
you can string together in a shell script to roll your own custom vector converter. The
result is very flexible, allowing you to use as many or as few features as you
like and to add your own programs to the pipeline to implement your own
features.

Normally, you want to put your command in a shell script. [Click here for example implementing the above command.](examples/simple-x1.sh)

Performance
--------------

EVCD
-----

EVCD is not supported, but check out [evcd2vcd](https://www.mankier.com/1/evcd2vcd), part of the excellent [gtkwave](http://gtkwave.sourceforge.net/) package.

twinwave
----------

A good way to see how your vcds are changed in convesion is [twinwave](https://www.mankier.com/1/twinwave), part of the excellent [gtkwave](http://gtkwave.sourceforge.net/) package.
