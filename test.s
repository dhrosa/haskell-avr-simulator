LDI R16, 0xF8

loop:
        INC R16
        OUT 0x2D, R16
        IN R31, 0x2D
        BREQ end
        RJMP loop

end:
        ORI R31, 0x01
