LDI R16, 0xF8

loop:
        INC R16
        BREQ end
        RJMP loop

end:
        ORI R31, 0x01