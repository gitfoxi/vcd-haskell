
class Field (object):
    def __init__(self, nm, lsb, length, rst="-", typ="-", desc=""):
        self.name = nm
        self.lsb = lsb
        self.length = length
        self.reset_value = rst
        self.typical_value = typ
        self.description = desc

    def msb(self):
        return self.lsb + self.length - 1

    def full_name(self):
        return "%s[%s]" % (self.name, self.bus_range())

    def bus_range(self):
        if self.length == 1:
            return "%d" % self.lsb
        return "%d:%d" % (self.msb(), self.lsb)

    def value(self, data):
        return getField((self.lsb, self.length), data)

class Register (object):
    def __init__(self, nm, did, addr, fields):
        self.name = nm
        self.did = did
        self.address = addr
        self.fields = fields

def mkFullAddr(did, addr):
    return addr + (did << 44) + (1 << 52)

def getField((lsb, length), reg):
    return (reg >> lsb) & ((1 << length) - 1)

registers = {
    mkFullAddr(0x18, 0x0080800020) : Register(
        "L2C_OCI_CTL", 0x18, 0x0080800020, [
            Field("ENAOCI", 0, 4, "F", "0", "Enable CCPI processing (one bit per node_id)"),
            Field("GKSEGNODE", 4, 2, "-", "0", "Initialized to the OCX_COM_NODE[ID] value on reset, which will equal the OCI_NODE_ID pins on a cold reset, but could be something else on a chip warm or soft reset; writable by software"),
        ]
    ),
    mkFullAddr(0x18, 0x0021000088) : Register(
        "VRM0_TS_TEMP_NOFF_MC", 0x18,  0x0021000088, [
            Field("NOFF", 0, 11, "0", "0", "N cycle count offset, used to subtract the appropriate count from N cycle. It should be such that at 0 degrees C, the difference between NOFF and NCYCLE is 0."),
            Field("MC", 16, 12, "BB8", "-", "MC value, default is 3000 decimal."),
        ])
}
