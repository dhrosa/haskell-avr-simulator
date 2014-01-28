LDI R30, 0x10
LDI R31, 0x00

LDI R16, 0x01
LDI R17, 0x02
LDI R18, 0x03

test_ldd:
        LDD R0, Z+0
        LDD R0, Z+1
        LDD R0, Z+2
        
