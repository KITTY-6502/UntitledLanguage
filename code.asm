#.cpu kitty

_INITCODE
    sei
    # Init system
    ldx $FF; txs
    stz [$70F0]; stz [$70F1]; stz [$70F2]; stz [$70F3]
    jsr [ClearScreen]
    lda 0; ldx TEMP
    __clrzp
        sta <$00+X>
    inc X; bne (clrzp)
    ldx 0
    __clrvar
        sta [$0200+X]
    inc X; bne (clrvar)
    # Init Variables
    #lda PROGRAM.lo; sta <PC+0>
    #lda PROGRAM.hi; sta <PC+1>
    lda $0300.lo; sta <PC+0>
    lda $0300.hi; sta <PC+1>
    ___randomseed
        clc
        lda <RSEED+0>; adc 1; sta <RSEED+0>
        lda <RSEED+1>; adc 0; sta <RSEED+1>
    bit [$7010]; bmi (randomseed)
    lda <RSEED+0>; bne (randomvalid)
    lda <RSEED+1>; bne (randomvalid)
        lda 1; sta <RSEED>
    ___randomvalid
_Run
    #inc [$6FFF]
    # Print stacks
    #clc; lda <OP1_Stack_PTR>; adc $30; sta [$681F]
    #clc; lda <OP2_Stack_PTR>; adc $30; sta [$683F]
    #ldx 0
    #__printOP1
    #    lda [OP1_Stack+X]
    #    cpx <OP1_Stack_PTR>; beq (print); bcc (print)
    #    lda ' '
    #    ___print
    #    sta [$6800+X]
    #inc X; cpx 8; bne (printOP1)
    #ldx 0
    #__printOP2
    #    lda [OP2_Stack+X]
    #    cpx <OP2_Stack_PTR>; beq (print); bcc (print)
    #    lda ' '
    #    ___print
    #    sta [$6820+X]
    #inc X; cpx 8; bne (printOP2)
    #ldx 50
    #__wait
    #    wai
    #dec X; bne (wait)
    # Interpret Thing
    __ATEND
    bit [$7040]; bpl (noescape)
        jmp [CODECONTINUE]
    __noescape
    
    lda <OP>; cmp ';'; bne (continue)
        jmp [ATEND]
    __continue
    
    ldy 0; lda [<PC>+Y]
    bne (notover)
        jsr [iopexecute]
        #stp
        jmp [ATEND]
    ___notover
    asl A; tax
    jmp [[ITOKENS+X]]
    
    jmp [Run]
    
    __ITOKENS
    #control
    .word ignore    # $00
    .word ignore    # $01
    .word ignore    # $02
    .word ignore    # $03
    .word ignore    # $04
    .word ignore    # $05
    .word ignore    # $06
    .word ignore    # $07
    .word ignore    # $08
    .word ignore    # $09
    .word endcmd    # $0A New Line
    .word ignore    # $0B
    .word ignore    # $0C
    .word ignore    # $0D
    .word ignore    # $0E
    .word ignore    # $0F
    .word ignore    # $10
    .word ignore    # $11
    .word ignore    # $12
    .word ignore    # $13
    .word ignore    # $14
    .word ignore    # $15
    .word ignore    # $16
    .word ignore    # $17
    .word ignore    # $18
    .word ignore    # $19
    .word ignore    # $1A
    .word ignore    # $1B
    .word ignore    # $1C
    .word ignore    # $1D
    .word ignore    # $1E
    .word ignore    # $1F
    # CHARACTERS
    .word ignore     # $20
    .word iflip      # $21 !
    .word istring    # $22 "
    .word iload      # $23 #
    .word ihexnumber # $24 $
    .word ignore     # $25 %
    .word ignore     # $26 &
    .word iconvert   # $27 '
    .word icondstart # $28 (
    .word icondend   # $29 )
    .word ignore     # $2A *
    .word iopset     # $2B + sum
    .word ignore     # $2C ,
    .word iopset     # $2D - sub
    .word ignore     # $2E .
    .word ignore     # $2F /
    .word ignore    # $30 0
    .word ignore    # $31 1
    .word ignore    # $32 2
    .word ignore    # $33 3
    .word ignore    # $34 4
    .word ignore    # $35 5
    .word ignore    # $36 6
    .word ignore    # $37 7
    .word ignore    # $38 8
    .word ignore    # $39 9
    .word iopset    # $3A : write
    .word endcmd    # $3B ;
    .word iopset    # $3C <
    .word iopset    # $3D =
    .word iopset    # $3E >
    .word ignore    # $3F ?
    .word ignore    # $40 @
    # Variables A-Z
    .word variable  # $41
    .word variable  # $42
    .word variable  # $43
    .word variable  # $44
    .word variable  # $45
    .word variable  # $46
    .word variable  # $47
    .word variable  # $48
    .word variable  # $49
    .word variable  # $4A
    .word variable  # $4B
    .word variable  # $4C
    .word variable  # $4D
    .word variable  # $4E
    .word variable  # $4F
    .word variable  # $50
    .word variable  # $51
    .word variable  # $52
    .word variable  # $53
    .word variable  # $54
    .word variable  # $55
    .word variable  # $56
    .word variable  # $57
    .word variable  # $58
    .word variable  # $59
    .word variable  # $5A
    #
    .word ignore    # $5B
    .word ignore    # $5C
    .word ignore    # $5D
    .word ignore    # $5E
    .word ignore    # $5F
    .word ignore    # $60 `
    # SYSTEM FUNCTIONS
    .word ignore    # $61 a
    .word ignore    # $62 b
    .word ignore    # $63 c
    .word ignore    # $64 d
    .word ignore    # $65 e
    .word ifill     # $66 (f)lip
    .word ignore    # $67 g
    .word ignore    # $68 h
    .word ignore    # $69 i
    .word ignore    # $6A j
    .word ignore    # $6B k
    .word ignore    # $6C l
    .word ignore    # $6D m
    .word ignore    # $6E n
    .word ignore    # $6F o
    .word ignore    # $70 p
    .word ignore    # $71 q
    .word irandom   # $72 (r)andom
    .word ignore    # $73 s
    .word ignore    # $74 t
    .word ignore    # $75 u
    .word ignore    # $76 v
    .word iwait     # $77 (w)ait
    .word ignore    # $78 x
    .word ignore    # $79 y
    .word ignore    # $7A z
    #
    .word ignore    # $7B {
    .word ignore    # $7C |
    .word ignore    # $7D }
    .word ignore    # $7E ~
    .word ignore    # $7F
    
    __variable
        sec; sbc 'A'; sta <r1>
        lda $02; sta <r0>
        jsr [PushR0]
        jsr [PushR1]
    jmp [ignore]
    __ignore
        inc <PC+0>; bne (noinc1)
            inc <PC+1>
        ___noinc1
    jmp [Run]
    __endcmd
        jsr [iendcmd]
    jmp [Run]
    __iendcmd
        lda <OP>; beq (continue)
            jsr [iopexecute]
            #jmp [Run]
        ___continue
        inc <PC+0>; bne (noinc1)
            inc <PC+1>
        ___noinc1
        stz <OP>; stz <Cur_Stack>
        stz <OP1_Stack_PTR>; stz <OP2_Stack_PTR>
    rts
    
    __icondstart
        stz <r4>
        jsr [iopexecute]
        ___noop
        jsr [PullR0]; bne (endcmd)
        ldy 0
        lda 1; sta <r0>
        ___loop
        inc <PC+0>; bne (noinc1)
            inc <PC+1>
        ____noinc1
        lda [<PC>+Y]
        cmp '('; beq (up)
        cmp ')'; beq (down)
        cmp '"'; beq (string)
        bra (loop)
        ____string
            inc <PC+0>; bne (noinc)
                inc <PC+1>
            _____noinc
        lda [<PC>+Y]; cmp '"'; bne (string)
        bra (loop)
        ____up
            inc <r0>
        bra (loop)
        ____down
            dec <r0>
        bne (loop)
        stz <Cur_Stack>; stz <OP1_Stack_PTR>; stz <OP2_Stack_PTR>
    jmp [endcmd]
    __icondend
        jsr [iopexecute]
        ___noop
        jsr [PullR0]; beq (endcmd)
        ldy 0
        lda 1; sta <r0>
        ___loop
        sec
        lda <PC+0>; sbc 1; sta <PC+0>
        lda <PC+1>; sbc 0; sta <PC+1>
        lda [<PC>+Y]
        
        cmp '('; beq (down)
        cmp ')'; beq (up)
        cmp '"'; beq (string)
        bra (loop)
        ____string
            sec
            lda <PC+0>; sbc 1; sta <PC+0>
            lda <PC+1>; sbc 0; sta <PC+1>
        lda [<PC>+Y]; cmp '"'; bne (string)
        bra (loop)
        ____up
            inc <r0>
        bra (loop)
        ____down
            dec <r0>
        bne (loop)
        ____noinc1
    jmp [endcmd]
        

    # Else, it is a value

jmp [Run]
##
__iflip
    jsr [iopexecute]
    inc <PC+0>; bne (noinc1)
        inc <PC+1>
    ___noinc1
    #jsr [iopexecute]
    jsr [PullR0]
    ldx 1
    lda <r0>; beq (continue)
        dec X
    ___continue
    stx <r0>; jsr [PushR0]
    

jmp [Run]
__istring
    ldx 0; ldy 0
    ___loop
        inc <PC+0>; bne (noinc1)
            inc <PC+1>
        ____noinc1
        lda [<PC>+Y]; cmp '"'; beq (break)
        cpx STACK_SIZE-1; beq (loop)
        inc X
        sta <TEMP_STACK+X>
    bra (loop)
    ___break
    ___pushloop
        cpx 0; beq (end)
        psh X
        lda <TEMP_STACK+X>; sta <r0>; jsr [PushR0]
        pul X
    dec X; bra (pushloop)
    ___end
    inc <PC+0>; bne (noinc1)
        inc <PC+1>
    ____noinc1
jmp [Run]


__ihexnumber
    ldy 0
    ___ihexnumberloop
    # first nibble
    #inc <PC+0>; bne (noinc1)
    #    inc <PC+1>
    inc Y
    ___noinc1
    lda [<PC>+Y]
    sec 
    sbc $30; bcc (break)
    cmp $0A; bcc (int)
        #bra (break)
    sec
    sbc $11; bcc (break)
    cmp $06; bcc (hex)
        bra (break)
    ____hex
    clc; adc $0A
    ____int
    sta <r0>
    
    # second nibble
    inc Y
    #inc <PC+0>; bne (noinc2)
    #    inc <PC+1>
    ___noinc2
    lda [<PC>+Y]
    sec
    sbc $30; bcc (endnibble)
    cmp $0A; bcc (int)
        #bra (endnumber)
    sec
    sbc $11; bcc (endnibble)
    cmp $06; bcc (hex)
        bra (endnibble)
    ____hex
    clc; adc $0A
    ____int
    rol <r0>; rol <r0>; rol <r0>; rol <r0>
    ora <r0>; sta <r0>
    jsr [PushR0]
bra (ihexnumberloop)
    ___endnibble
    clc; tya; adc <PC+0>; sta <PC+0>
    lda <PC+1>; adc 0; sta <PC+1>
    jsr [PushR0]
jmp [Run]
___break
    clc; tya; adc <PC+0>; sta <PC+0>
    lda <PC+1>; adc 0; sta <PC+1>
    #inc [$6BFE]
jmp [Run]
##
__iload
    inc <PC+0>; bne (noinc1)
        inc <PC+1>
    ___noinc1
    # works regardless of op
    stz <r2>
    ___loop
    #inc [$6BFF]
    jsr [PullR0]
    jsr [PullR1]
    ldy 0; lda [<r0>+Y]
    ldx <r2>; sta [TEMP_STACK+X]; inc <r2>
    
    lda <Cur_Stack>; beq (checkOP1)
    ___checkOP2
    lda <OP2_Stack_PTR>; beq (end)
bra (loop)
    ___checkOP1
    lda <OP1_Stack_PTR>; beq (end)
bra (loop)
___end
    ldx 0
    ___uploop
        lda [TEMP_STACK+X]; sta <r0>
        phx; jsr [PushR0]; plx
    inc X; cpx <r2>; bne (uploop)
jmp [Run]

__iconvert
    ldy 0; stz <r2>
    ___loop
    lda <Cur_Stack>; beq (op1)
    ____op2
        lda <OP2_Stack_PTR>; beq (done)
    bra (continue)
    ____op1
        lda <OP1_Stack_PTR>; beq (done)
    ____continue
    jsr [PullR0]
    inc <r2>
        lda <r0>; and %0000_1111; tax
        lda [hextostring+X]; ldx <r2>; sta <TEMP_STACK+X>
    inc <r2>; ldy <r2>
        lda <r0>; lsr A; lsr A; lsr A; lsr A; tax
        lda [hextostring+X]; ldx <r2>; sta <TEMP_STACK+X>
    bra (loop)
    ___done
    ldx 0
    ___pushloop
        lda <TEMP_STACK+1+X>; sta <r0>
        phx; jsr [PushR0]; plx
        inc X; cpx <r2>; beq (return)
    bra (pushloop)
    ___return
jmp [ignore]
___hextostring
.byte '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'

__ifill
    stz <Cur_Stack>
    jsr [PullR0]
    ldx 0
    ___loop1
        sta [$6C00+X]
        sta [$6D00+X]
        sta [$6E00+X]
        sta [$6F00+X]
    inc X; bne (loop1)
    
    lda <OP1_Stack_PTR>; beq (return)
    jsr [PullR0]
    ___loop2
        sta [$6800+X]
        sta [$6900+X]
        sta [$6A00+X]
        sta [$6B00+X]
    inc X; bne (loop2)
    ___return
jmp [ignore]

__iwait
    jsr [PullR0]; ldx <r0>
    ___loop
        wai
    dec X; bne (loop)
jmp [ignore]

__irandom
    jsr [PullR0]; dec <r0>
    ___findvalue
    ldy 8 # Iteration couunt (generate 8 bits)
    lda <RSEED+0>
    ___loop
    asl A; rol <RSEED+1>
    bcc (noxor)
        xor $39
    ___noxor
    dec Y; bne (loop)
    sta <RSEED+0>; cmp <r0>; beq (cont); bcs (findvalue)
    ___cont
    sta <r0>
    jsr [PushR0]
jmp [ignore]
##
##
__iopset
    lsr A; pha
    lda <OP>; beq (continue)
        jsr [iopexecute]
    ___continue
    pla; sta <OP>; sta <Cur_Stack>
    inc <PC+0>; bne (noinc1)
        inc <PC+1>
    ___noinc1
jmp [Run]

__iopexecute
    stz <r0>
    lda <OP>
    cmp '+'; beq (opsum)
    cmp '-'; beq (opsub)
    cmp ':'; beq (opwrite)
    cmp '<'; beq (oplesser)
    cmp '='; beq (opequal)
    cmp '>'; beq (opgreater)
    #cmp '?'; beq (opcond)
rts
___opgreater
    jmp [iopgreater]
___opequal
    jmp [iopequal]
___oplesser
    jmp [ioplesser]
___opsum
    jmp [iopsum]
___opwrite
    jmp [iopwrite]
___opsub
    jmp [iopsub]

___ioplesser
    sec
    stz <r2>
    ____loop
     lda <OP1_Stack_PTR>; bne (continue)
     lda <OP2_Stack_PTR>; bne (continue)
        bra (exitloop)
    ____continue
        inc <r2>
        
        lda 0; sta <Cur_Stack>
        jsr [PullR0]
        lda 1; sta <Cur_Stack>
        jsr [PullR1]
        
        lda <r0>; sbc <r1>; 
        ldx <r2>; sta <r3+X>
     bra (loop)
     ____exitloop
    # Upload Result
    stz <r0>
    bcs (notlesser)
        inc <r0>
    ____notlesser
    stz <Cur_Stack>
    jsr [PushR0]
jmp [end]

___iopgreater
    sec
    stz <r2>
    ____loop
     lda <OP1_Stack_PTR>; bne (continue)
     lda <OP2_Stack_PTR>; bne (continue)
        bra (exitloop)
    ____continue
        inc <r2>
        
        lda 0; sta <Cur_Stack>
        jsr [PullR0]
        lda 1; sta <Cur_Stack>
        jsr [PullR1]
        
        lda <r0>; sbc <r1>; 
        ldx <r2>; sta <r3+X>
     bra (loop)
     ____exitloop
    # Upload Result
    stz <r0>
    bcc (notgreater)
    lda 0; ldx <r2>
    ____testloop
      ora <r3+X>
    dec X; bne (testloop)
    and $FF
    beq (notgreater)
        inc <r0>
    ____notgreater
    stz <Cur_Stack>
    jsr [PushR0]
jmp [end]

___iopequal
    sec
    stz <r2>
    ____loop
     lda <OP1_Stack_PTR>; bne (continue)
     lda <OP2_Stack_PTR>; bne (continue)
        bra (exitloop)
    ____continue
        inc <r2>
        
        lda 0; sta <Cur_Stack>
        jsr [PullR0]
        lda 1; sta <Cur_Stack>
        jsr [PullR1]
        
        lda <r0>; sbc <r1>; 
        ldx <r2>; sta <r3+X>
     bra (loop)
     ____exitloop
    # Upload Result
    stz <r0>
    bcc (notequal)
    lda 0; ldx <r2>
    ____testloop
      ora <r3+X>
    dec X; bne (testloop)
    and $FF
    bne (notequal)
        inc <r0>
    ____notequal
    stz <Cur_Stack>
    jsr [PushR0]
jmp [end]

___iopsub
    sec
    stz <r2>
    ____loop
     lda <OP1_Stack_PTR>; bne (continue)
     lda <OP2_Stack_PTR>; bne (continue)
        bra (exitloop)
    ____continue
        inc <r2>
        
        lda 0; sta <Cur_Stack>
        jsr [PullR0]
        lda 1; sta <Cur_Stack>
        jsr [PullR1]
        
        lda <r0>; sbc <r1>; 
        ldx <r2>; sta <r3+X>
     bra (loop)
     ____exitloop
     # Upload Result
     ldx <r2>
     stz <Cur_Stack>
     #lda '0'; sta [$6901]
     ____uploop
        phx
        lda <r3+X>; sta <r0>
        jsr [PushR0]
        #inc [$6901]
     plx; dec X; bne (uploop)
     #clc; lda <r2>; adc $30; sta [$6900]
jmp [end]

___iopsum
    clc
    stz <r2>
    ____loop
        inc <r2>
        
        lda 0; sta <Cur_Stack>
        jsr [PullR0]
        lda 1; sta <Cur_Stack>
        jsr [PullR1]
        
        lda <r0>; adc <r1>; 
        ldx <OP1_Stack_PTR>; sta <OP1_Stack+1+X>
     lda <OP1_Stack_PTR>; bne (loop)
     lda <OP2_Stack_PTR>; bne (loop)
     ____exitloop
     # Upload Result
     #ldx <r2>
     stz <Cur_Stack>
     lda <r2>; sta <OP1_Stack_PTR>
     #lda '0'; sta [$6901]
     #____uploop
     #   phx
     #   lda <r3+X>; sta <r0>
     #   jsr [PushR0]
     #   #inc [$6901]
     #plx; dec X; bne (uploop)
     #clc; lda <r2>; adc $30; sta [$6900]
jmp [end]
___iopwrite
    lda <OP1_Stack_PTR>; sta <r5>
    stz <r4>; lda 1; sta <Cur_Stack>
    ____op2loop
        inc <r4>
        jsr [PullR1]; lda <r1>; ldx <r4>; sta <TEMP_STACK+X>
        inc <r4>
        jsr [PullR1]; lda <r1>; ldx <r4>; sta <TEMP_STACK+X>
    lda <OP2_Stack_PTR>; bne (op2loop)
    
    
    ldx <r4>; lda <TEMP_STACK+X>; sta <r3>; dec <r4>
    ldx <r4>; lda <TEMP_STACK+X>; sta <r2>; dec <r4>
    
    ldy 0
    stz <Cur_Stack>
    ____loop
        jsr [PullR0]
        sta [<r2>+Y]
        lda <r4>; beq (noinc)
            ldx <r4>; lda <TEMP_STACK+X>; sta <r3>; dec <r4>
            ldx <r4>; lda <TEMP_STACK+X>; sta <r2>; dec <r4>
            ldy $FF
        ____noinc
    inc Y; lda <OP1_Stack_PTR>; bne (loop)
    ____break
    lda <r5>; sta <OP1_Stack_PTR>;
    #stz <OP1_Stack_PTR>;
    stz <OP2_Stack_PTR>
jmp [end]
___end
    stz <OP>; stz <Cur_Stack>
rts

    
#====================================
# Subroutines
_PushR0
    lda <Cur_Stack>; beq (op1)
    __op2
    lda STACK_SIZE; cmp <OP2_Stack_PTR>; beq (fail)
    inc <OP2_Stack_PTR>
    lda <r0>
    ldx <OP2_Stack_PTR>; sta <OP2_Stack+X>
rts
    __op1
    lda STACK_SIZE; cmp <OP1_Stack_PTR>; beq (fail)
    inc <OP1_Stack_PTR>
    lda <r0>
    ldx <OP1_Stack_PTR>; sta <OP1_Stack+X>
rts
__fail
rts
_PushR1
    lda <Cur_Stack>; beq (op1)
    __op2
    lda STACK_SIZE; cmp <OP2_Stack_PTR>; beq (fail)
    inc <OP2_Stack_PTR>
    lda <r1>
    ldx <OP2_Stack_PTR>; sta <OP2_Stack+X>
rts
    __op1
    lda STACK_SIZE; cmp <OP1_Stack_PTR>; beq (fail)
    inc <OP1_Stack_PTR>
    lda <r1>
    ldx <OP1_Stack_PTR>; sta <OP1_Stack+X>
rts
__fail
rts

# Subroutines
_PullR0
    lda <Cur_Stack>; beq (op1)
    
    __op2
    ldx <OP2_Stack_PTR>; beq (stackend)
    dec <OP2_Stack_PTR>
    lda <OP2_Stack+X>
    sta <r0>
rts
    __op1
    ldx <OP1_Stack_PTR>; beq (stackend)
    dec <OP1_Stack_PTR>
    lda <OP1_Stack+X>
    sta <r0>
rts
__stackend
    stz <r0>
rts
# Subroutines
_PullR1
    lda <Cur_Stack>; beq (op1)
    
    __op2
    lda <OP2_Stack_PTR>; beq (stackend)
    tax
    lda <OP2_Stack+X>
    dec X; stx <OP2_Stack_PTR>
    sta <r1>
rts
    __op1
    lda <OP1_Stack_PTR>; beq (stackend)
    tax
    lda <OP1_Stack+X>
    dec X; stx <OP1_Stack_PTR>
    sta <r1>
rts
__stackend
    stz <r1>
rts

_ClearScreen
    ldx $00
    __loop
        lda ' '
        sta [CHR+$000+X]; sta [CHR+$100+X]; sta [CHR+$200+X]; sta [CHR+$300+X]
        lda $F0
        sta [PAL+$000+X]; sta [PAL+$100+X]; sta [PAL+$200+X]; sta [PAL+$300+X]
    inc X; bne (loop)
rts

#_PROGRAM
#.bin "code.ks"
#.byte 0

#.pad [VECTORS]
#.word NMI
#.word RESET
#.word IRQ