start:
        LDI R20, 0x10
        OUT 0x3D, R20
        LDI R30, 0x80
        LDI R31, 0x7F
        RCALL func
        RJMP end
func:
        ADD R31, R30
        RET

end:
        CLR R30
        CLR R31
