
vcd-haskell
==============

Convert VCD to SmartScale 93k test vectors.

(Note: pre-alpha - maybe wait a few days for a decent release)

Features
----------

* Fast
* Free
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
* Linux, Mac, Windows



Install
---------

### New Linux & Mac

* Install [stack](https://docs.haskellstack.org/en/stable/README/)
* Use stack to build and install

```
    stack setup
    stack build
    stack install
```

* Create a symlink to libaldc (only if you want VECC compression) - TODO

### Old Linux

You might want to run this on your smarTest workstation with RedHat 5.8, 5.6 or
even an old 32-bit RedHat 3. That's very convenient to be able to tweak your
vector conversions while working on the tester.

The problem is that it's hard to get GHC (the haskell compiler) and
current tools needed to build on old systems. I have done it and I will share
compiled binaries with anyone who asks me for them.

### Windows

Install the [native Windows stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/#windows) and follow the instructions above for Linux & Mac.

You could, instead, almost use the new Windows Subsystem for
Linux. There's a current bug that makes Haskell slow on WSL so you might
want to wait until [until this bug is closed.](https://github.com/Microsoft/BashOnWindows/issues/1671)

Scripting under windows is a little different. You may want to setup [Ubuntu under VirtualBox](https://help.ubuntu.com/community/VirtualBox) for a more consistent experience.


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


TODO
-----

* Multiport burst
* Time-based sampling
* Non-standard waveforms
* JTAG state comments
* Old 93k with VM memory (C400, P1000, PinScale running in backwards-compatible memory mode)
* Vtran configuration porting tool
* Multi-cycle repeat compression


LICENSE
---------

Copyright (C) 2016, 2017  Michael Fox

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
