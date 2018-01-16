#!/usr/bin/env python2


# // The format for writing a command is
# [127]     1=do command, 0=do nothing
# [126:124] not used, should be 0
# [123:116] destid
# [115:108] mask
# [107:104] command
# [103:64]  address
# [63:0]    write data

# // The format for reading out
# [127]     command accepted
# [126]     read complete, read is valid
# [125:64]  not used, will be 0
# [63:0]    read data

# 2590 IR: c DRtdi: 818ff40021000088000000000abe0000 DRtdo: c0000000000000000000ffffffffffff
# 2741 IR: c DRtdi: 818ff30021000088000000000abe0000 DRtdo: c0000000000000000000ffffffffffff
# 2887 IR: c DRtdi: 018ff30021000088000000000abe0000 DRtdo: c000000000000000000000000abe0000

from functools import partial
from sys import stdin
import octeon_regs as regs

def kvsFromList(ts):
    r =[]
    while len(ts) > 1:
        k = ts.pop(0)
        v = ts.pop(0)
        r.append((k,v))
    return r


class Dr (dict):
    def __init__(self, s):
        self.parse(s)

    def parse(self, s):
        ts = s.split()
        self.cycle = int(ts.pop(0))
        kvs = kvsFromList(ts)
        map(self.parse1, kvs)

    def parse1(self, (k,v)):
        self[k.rstrip(':')] = int(v, 16)

    def __str__(self):
        return "%d DR IR: %X tdi: %X tdo: %X" % (self.cycle, self['IR'], self['DRtdi'], self['DRtdo'])

irType = {
    0xc : "CSR"
}

DO_COMMAND = (127,1)
READ_IS_VALID = (126,1)
CMD = (104,4)
COMMAND_ACCEPTED = (127,1)
READ_DATA = (0,64)
WRITE_DATA = (0,64)
ADDRESS = (64,40)
DEST_ID = (116,8)
MASK = (108,8)

def msb((lsb,length)):
    return lsb + length - 1

CMD_WRITE = 4
CMD_READ  = 3

def getField((lsb, length), reg):
    return (reg >> lsb) & ((1 << length) - 1)

class Octeon(object):
    def __init__(self, dr):
        self.dr = dr
        self.irType = irType.get(dr['IR'], '?')
        self.msb = dr.cycle

        if self.irType == 'CSR':
            self.parse_csr(dr)

    def parse_csr(self,dr):
        self.doCommand = getField(DO_COMMAND, dr['DRtdi'])
        self.readIsValid = getField(READ_IS_VALID, dr['DRtdo'])
        self.cmdAcceped = getField(COMMAND_ACCEPTED, dr['DRtdo'])
        self.readData = getField(READ_DATA, dr['DRtdo'])
        self.cmd = getField(CMD, dr['DRtdi'])
        self.address = getField(ADDRESS, dr['DRtdi'])
        self.writeData = getField(WRITE_DATA, dr['DRtdi'])
        self.destId = getField(DEST_ID, dr['DRtdi'])
        self.mask = getField(MASK, dr['DRtdi'])

    def lsb(self):
        return self.msb - 127

    def __str__(self):
        # If not an Octeon CSR we don't know how to decode
        if self.irType != 'CSR':
            return str(self.dr)
        if self.cmd == CMD_WRITE:
            return field_comments(self.destId, self.address, self.lsb(), self.writeData) + \
"""%d dat: %016X
%d addr: %010X
%d cmd: %01X
%d mask: %02X
%d did: %02X
%d WR did: %02X addr: %010X dat: %016X""" % (
                self.lsb() + msb(WRITE_DATA), self.writeData,
                self.lsb() + msb(ADDRESS), self.address,
                self.lsb() + msb(CMD), self.cmd,
                self.lsb() + msb(MASK), self.mask,
                self.lsb() + msb(DEST_ID), self.destId,
                self.msb, self.destId, self.address, self.writeData) + register_comment(self.destId, self.address)
        elif self.cmd == CMD_READ:
            if self.doCommand:
                return \
"""%d addr: %010X
%d cmd: %01X
%d did: %02X
%d RQ did: %02X addr: %010X""" % (
                self.lsb() + msb(ADDRESS), self.address,
                self.lsb() + msb(CMD), self.cmd,
                self.lsb() + msb(DEST_ID), self.destId,
                self.msb, self.destId, self.address) + register_comment(self.destId, self.address)
            else:
                return field_comments(self.destId, self.address, self.lsb(), self.readData) + \
"""%d dat: %016X
%d cmd: %01X
%d RD dat: %016X""" % (
                self.lsb() + msb(READ_DATA), self.readData,
                self.lsb() + msb(CMD), self.cmd,
                self.msb, self.readData) + register_comment(self.destId, self.address)
        else:
            return str(self.dr)

def register_comment(did, addr):
    fullAddress = regs.mkFullAddr(did, addr)
    if fullAddress not in regs.registers:
        return ''
    return ' ' + regs.registers[fullAddress].name

def field_comments(did, addr, lsb, data):
    fullAddress = regs.mkFullAddr(did, addr)
    if fullAddress not in regs.registers:
        return ''
    fields = regs.registers[fullAddress].fields
    return '\n'.join(map(partial(field_comment, lsb, data), fields)) + '\n'

def field_comment(lsb, data, field):
    return "%d %s %X" % (lsb + field.msb(), field.full_name(), field.value(data))

print "0 All Comments at MSB of field"
for l in stdin.readlines():
    print Octeon(Dr(l))
