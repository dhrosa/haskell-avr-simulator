ORI R31, 0x10
MOV R0, R31
OUT 0x3D, R0

CLR R0
loop:
        RCALL func
        RJMP loop
        
func:
        INC R0
        RET
