#.cpu kitty
_ClearScreen
    ldx $00
    __loop
        lda ' '
        sta [CHR+$000+X]; sta [CHR+$100+X]; sta [CHR+$200+X]; sta [CHR+$300+X]
        lda $F0
        sta [PAL+$000+X]; sta [PAL+$100+X]; sta [PAL+$200+X]; sta [PAL+$300+X]
    inc X; bne (loop)
rts

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
    ldy 0
    bra (tRUN)
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
    __tEND
    tya; clc; adc 1
    adc <PC+0>; sta <PC+0>
    lda 0
    adc <PC+1>; sta <PC+1>
    __tRUN
    bit [$7040]; bpl (noescape)
        jmp [CODEEND]
    __noescape
    
    ldy 0; lda [<PC>+Y]
    asl A; tax
    jmp [[TOKENS+X]]
    
    __TOKENS
    #control
    .word tEOF       # $00
    .word tIgnore    # $01
    .word tIgnore    # $02
    .word tIgnore    # $03
    .word tIgnore    # $04
    .word tIgnore    # $05
    .word tIgnore    # $06
    .word tIgnore    # $07
    .word tIgnore    # $08
    .word tIgnore    # $09
    .word tEndCmd    # $0A New Line
    .word tIgnore    # $0B
    .word tIgnore    # $0C
    .word tIgnore    # $0D
    .word tIgnore    # $0E
    .word tIgnore    # $0F
    .word tIgnore    # $10
    .word tIgnore    # $11
    .word tIgnore    # $12
    .word tIgnore    # $13
    .word tIgnore    # $14
    .word tIgnore    # $15
    .word tIgnore    # $16
    .word tIgnore    # $17
    .word tIgnore    # $18
    .word tIgnore    # $19
    .word tIgnore    # $1A
    .word tIgnore    # $1B
    .word tIgnore    # $1C
    .word tIgnore    # $1D
    .word tIgnore    # $1E
    .word tIgnore    # $1F
    # CHARACTERS
    .word tIgnore        # $20
    .word tInv           # $21 !
    .word tString        # $22 "
    .word tLoad          # $23 #
    .word tHexNumber     # $24 $
    .word tIgnore        # $25 %
    .word tIgnore        # $26 &
    .word tHexAscii      # $27 '
    .word tCondStart    # $28 (
    .word tCondEnd      # $29 )
    .word tIgnore     # $2A *
    .word tOpcode     # $2B + sum
    .word tIgnore     # $2C ,
    .word tOpcode     # $2D - sub
    .word tIgnore     # $2E .
    .word tIgnore     # $2F /
    .word tDecNumber    # $30 0
    .word tDecNumber    # $31 1
    .word tDecNumber    # $32 2
    .word tDecNumber    # $33 3
    .word tDecNumber    # $34 4
    .word tDecNumber    # $35 5
    .word tDecNumber    # $36 6
    .word tDecNumber    # $37 7
    .word tDecNumber    # $38 8
    .word tDecNumber    # $39 9
    .word tOpcode    # $3A : write
    .word tEndCmd    # $3B ;
    .word tOpcode    # $3C <
    .word tOpcode    # $3D =
    .word tOpcode    # $3E >
    .word tIgnore    # $3F ?
    .word tIgnore    # $40 @
    # Variables A-Z
    .word tVariable  # $41
    .word tVariable  # $42
    .word tVariable  # $43
    .word tVariable  # $44
    .word tVariable  # $45
    .word tVariable  # $46
    .word tVariable  # $47
    .word tVariable  # $48
    .word tVariable  # $49
    .word tVariable  # $4A
    .word tVariable  # $4B
    .word tVariable  # $4C
    .word tVariable  # $4D
    .word tVariable  # $4E
    .word tVariable  # $4F
    .word tVariable  # $50
    .word tVariable  # $51
    .word tVariable  # $52
    .word tVariable  # $53
    .word tVariable  # $54
    .word tVariable  # $55
    .word tVariable  # $56
    .word tVariable  # $57
    .word tVariable  # $58
    .word tVariable  # $59
    .word tVariable  # $5A
    #
    .word tIgnore    # $5B
    .word tCondStart    # $5C \
    .word tIgnore    # $5D
    .word tIgnore    # $5E
    .word tIgnore    # $5F
    .word tIgnore    # $60 `
    # SYSTEM FUNCTIONS
    .word tIgnore    # $61 a
    .word tIgnore    # $62 b
    .word tIgnore    # $63 c
    .word tIgnore    # $64 d
    .word tIgnore    # $65 e
    .word tOpcode     # $66 (f)lip
    .word tIgnore    # $67 g
    .word tIgnore    # $68 h
    .word tIgnore    # $69 i
    .word tIgnore    # $6A j
    .word tIgnore    # $6B k
    .word tIgnore    # $6C l
    .word tIgnore    # $6D m
    .word tIgnore    # $6E n
    .word tIgnore    # $6F o
    .word tIgnore    # $70 p
    .word tIgnore    # $71 q
    .word tRandom    # $72 (r)andom
    .word tIgnore    # $73 s
    .word tIgnore    # $74 t
    .word tIgnore    # $75 u
    .word tIgnore    # $76 v
    .word tOpcode     # $77 (w)ait
    .word tIgnore    # $78 x
    .word tIgnore    # $79 y
    .word tIgnore    # $7A z
    #
    .word tIgnore    # $7B {
    .word tIgnore    # $7C |
    .word tIgnore    # $7D }
    .word tNot       # $7E ~
    .word tIgnore    # DELETE
# Tokens
__tEOF
    lda <OP>; beq (next); jmp [oRUN]
    ___next
    bit [$7040]; bpl (next)
jmp [CODEEND]
__tIgnore
jmp [tEND]
__tOpcode
    lda <OP>; beq (next); jmp [oRUN]
    ___next
    txa; lsr A; sta <OP>
    tsx; stx <OPStack>
    ldx $7F; txs
jmp [tEND]
__tVariable
    # hi byte
    ldx 0
    phx
    # lo byte
    lsr A; clc; adc $80
    pha
    
jmp [tEND]
__tEndCmd
    lda <OP>; beq (next); jmp [oRUN]
    __next
    ldx $FF; txs
jmp [tEND]
__tCondStart
    lda <OP>; beq (next); jmp [oRUN]
    ___next
    tsx; txa; ora $80; tay
    lda 0
    ___orStack
    inc X; inc Y; beq (checkIfTrue)
    ora [$0100+X]
    bra (orStack)
    ___checkIfTrue
    # Y is 0 at this point
    ora $00
    #sta [$6801]
    bne (True)
    ___False
    # X is Indent Counter
    ldx 1
    ___FalseLoop
    # Increment Counter
    clc
    lda <PC+0>; adc 1; sta <PC+0>
    lda <PC+1>; adc 0; sta <PC+1>
    
    lda [<PC>+Y]
    beq (endOfFile)
    cmp ')'; beq (close)
    cmp '\'; beq (closeMaybe)
    cmp '('; beq (open)
    cmp $22; beq (string)
    bra (FalseLoop)
    ____closeMaybe
        cpx 1; bne (FalseLoop)
    ____close
        dec X; beq (FoundFalse); beq (FalseLoop)
    ____open
        inc X; bra (FalseLoop)
    ____string
        clc
        lda <PC+0>; adc 1; sta <PC+0>
        lda <PC+1>; adc 0; sta <PC+1>
        
        lda [<PC>+Y]
        beq (endOfFile)
        cmp $22; beq (FalseLoop)
    bra (string)
    ___FoundFalse
    ldx $FF; txs
    lda <PC+0>; sta [$6821]
    lda <PC+1>; sta [$6820]
    jmp [tEND]
    
    ___True
    ldx $FF; txs
    jmp [tEND]
    ___endOfFile
    jmp [CODEEND]
__tCondEnd
    lda <OP>; beq (next); jmp [oRUN]
    ___next
    tsx; txa; ora $80; tay
    lda 0
    ___orStack
    inc X; inc Y; beq (checkIfTrue)
    ora [$0100+X]
    bra (orStack)
    ___checkIfTrue
    # Y is 0 at this point
    ora $00
    #sta [$6802]
    beq (False)
    ___True
    # X is Indent Counter
    ldx 1
    ___TrueLoop
    # Increment Counter
    sec
    lda <PC+0>; sbc 1; sta <PC+0>
    lda <PC+1>; sbc 0; sta <PC+1>
    
    lda [<PC>+Y]
    cmp ')'; beq (close)
    cmp '('; beq (open)
    cmp $22; beq (string)
    bra (TrueLoop)
    ____open
        dec X; bne (TrueLoop); bra (FoundTrue)
    ____close
        inc X; bra (TrueLoop)
    ____string
        sec
        lda <PC+0>; sbc 1; sta <PC+0>
        lda <PC+1>; sbc 0; sta <PC+1>
        
        lda [<PC>+Y]
        beq (endOfFile)
        cmp $22; beq (TrueLoop)
    bra (string)
    ___FoundTrue
    ldx $FF; txs
    jmp [tEND]
    
    
    ___False
    ldx $FF; txs
    jmp [tEND]
    ___endOfFile
    jmp [CODEEND]
jmp [tEND]
__tString
    ___findEnd
        inc Y; beq (ERROR)
    lda [<PC>+Y]; beq (endOfFile); cmp $22; bne (findEnd)
    sty <R0>
    ___upload
        dec Y; beq (done)
        lda [<PC>+Y]; pha
    bra (upload)
    ___done
    ldy <R0>
    ___ERROR
jmp [tEND]
___endOfFile
jmp [CODEEND]
__tHexNumber
    ___1stNibble
    stz <R0>    
    inc Y
    lda [<PC>+Y]
    bmi (done)
    cmp '9'+1; bcs (AtoF)
    sec
    sbc '0'; bcc (done)
    ____0to9
    bra (2ndNibble)
    ____AtoF
    ora %0010_0000  # turn lowercase
    cmp 'a'; bcc (done)
    cmp 'f'+1; bcs (done)
    sec; sbc 'a'-10
    ___2ndNibble
    sta <R0>
    inc Y
    lda [<PC>+Y]
    bmi (1stPush)
    cmp '9'+1; bcs (AtoF)
    sec
    sbc '0'; bcc (1stPush)
    ____0to9
    bra (2ndPush)
    ____AtoF
    ora %0010_0000  # turn lowercase
    cmp 'a'; bcc (1stPush)
    cmp 'f'+1; bcs (1stPush)
    sec; sbc 'a'-10
    ____2ndPush
    sta <R1>
    lda <R0>; asl A; asl A; asl A; asl A
    ora <R1>
    pha
    #sta [$6900]
    bra (1stNibble)
    ____1stPush
    lda <R0>; pha
    ___done
    dec Y
jmp [tEND]
__tDecNumber
    lda [<PC>+Y]
    sec; sbc '0'; pha
jmp [tEND]
__tLoad
    tsx; stx <R0>;
    txa; ora %0111_1111
    tax; txs
    ___loop
      cpx <R0>; beq (done)
      lda [$0100+X]; sta <R2+1>
      dec X; cpx <R0>; beq (done)
      lda [$0100+X]; sta <R2+0>
      dec X
      lda [<R2>+Y]; pha
    bra (loop)
    __done
jmp [tEND]

__tHexAscii
    tsx; stx <R0>; txa; ora %0111_1111; tax; stx <R1>
    
    #ldx <R1>
    ___copyLoop
    cpx <R0>; beq (copyDone)
        pla; sta [$00C0+X]
        dec X
    bra (copyLoop)
    ___copyDone
    ldx <R1>
    ___hexLoop
        # lo nibble
        lda [$00C0+X]; and $0F
        tay; lda [table+Y]; pha
        # hi nibble
        lda [$00C0+X]; lsr A; lsr A; lsr A; lsr A
        tay; lda [table+Y]; pha
        
    dec X; cpx <R0>; bne (hexLoop)
    
    ___done
    ldy 0
jmp [tEND]
___table
.byte '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'

__tInv
  stz <R0>
  tsx; bpl (operator)
  ___operand
  inc X; bpl (done)
    pla; ora <R0>; sta <R0>
  bra (operand)
  ___operator
  inc X; bmi (done)
    pla; ora <R0>; sta <R0>
  bra (operator)
  ___done
  lda <R0>; beq (zero)
  ___one
  lda 0; pha
jmp [tEND]
  ___zero
  lda 1; pha
jmp [tEND]

__tNot
    lda $EE; sta [$6C30]
    tsx; stx <R0> 
    txa; ora $80; tax
    ___loop
    inc X; beq (done)
        pla; xor $FF; pha; pla
    bra (loop)
    ___done
    ldx <R0>; txs
jmp [tEND]

__tRandom
  ldx 8
  lda <RSEED+0>
  ___loop
  asl A; rol <RSEED+1>
  bcc (noxor)
  ___xor
  xor $39
  ___noxor
  dec Y; bne (loop)
  # Store back in 0
  sta <RSEED+0>
  pha
jmp [tEND]

# ============================================
__OPCODES
  # Control Codes
  .word oIgnore       # 00
  .word oIgnore       # 01
  .word oIgnore       # 02
  .word oIgnore       # 03
  .word oIgnore       # 04
  .word oIgnore       # 05
  .word oIgnore       # 06
  .word oIgnore       # 07
  .word oIgnore       # 08
  .word oIgnore       # 09
  .word oIgnore       # 0A
  .word oIgnore       # 0B
  .word oIgnore       # 0C
  .word oIgnore       # 0D
  .word oIgnore       # 0E
  .word oIgnore       # 0F
  .word oIgnore       # 10
  .word oIgnore       # 11
  .word oIgnore       # 12
  .word oIgnore       # 13
  .word oIgnore       # 14
  .word oIgnore       # 15
  .word oIgnore       # 16
  .word oIgnore       # 17
  .word oIgnore       # 18
  .word oIgnore       # 19
  .word oIgnore       # 1A
  .word oIgnore       # 1B
  .word oIgnore       # 1C
  .word oIgnore       # 1D
  .word oIgnore       # 1E
  .word oIgnore       # 1F
  # Symbols
  .word oIgnore       # 20
  .word oIgnore       # 21
  .word oIgnore       # 22
  .word oIgnore       # 23
  .word oIgnore       # 24
  .word oOr           # 25  %
  .word oAnd          # 26  &
  .word oIgnore       # 27
  .word oIgnore       # 28
  .word oIgnore       # 29
  .word oIgnore       # 2A
  .word oAdd          # 2B  +
  .word oIgnore       # 2C
  .word oSub          # 2D  -
  .word oIgnore       # 2E
  .word oIgnore       # 2F
  .word oIgnore       # 30
  .word oIgnore       # 31
  .word oIgnore       # 32
  .word oIgnore       # 33
  .word oIgnore       # 34
  .word oIgnore       # 35
  .word oIgnore       # 36
  .word oIgnore       # 37
  .word oIgnore       # 38
  .word oIgnore       # 39
  .word oStore        # 3A  :
  .word oIgnore       # 3B
  .word oIgnore       # 3C
  .word oIgnore       # 3D
  .word oIgnore       # 3E
  .word oIgnore       # 3F
  # Uppercase
  .word oIgnore       # 40
  .word oIgnore       # 41
  .word oIgnore       # 42
  .word oIgnore       # 43
  .word oIgnore       # 44
  .word oIgnore       # 45
  .word oIgnore       # 46
  .word oIgnore       # 47
  .word oIgnore       # 48
  .word oIgnore       # 49
  .word oIgnore       # 4A
  .word oIgnore       # 4B
  .word oIgnore       # 4C
  .word oIgnore       # 4D
  .word oIgnore       # 4E
  .word oIgnore       # 4F
  .word oIgnore       # 50
  .word oIgnore       # 51
  .word oIgnore       # 52
  .word oIgnore       # 53
  .word oIgnore       # 54
  .word oIgnore       # 55
  .word oIgnore       # 56
  .word oIgnore       # 57
  .word oIgnore       # 58
  .word oIgnore       # 59
  .word oIgnore       # 5A
  .word oIgnore       # 5B
  .word oIgnore       # 5C
  .word oIgnore       # 5D
  .word oXor          # 5E  ^
  .word oIgnore       # 5F
  # Lowercase
  .word oIgnore       # 00
  .word oIgnore       # 01
  .word oIgnore       # 02
  .word oIgnore       # 03
  .word oIgnore       # 04
  .word oIgnore       # 05
  .word oIgnore       # 06
  .word oIgnore       # 07
  .word oIgnore       # 08
  .word oIgnore       # 09
  .word oIgnore       # 0A
  .word oIgnore       # 0B
  .word oIgnore       # 0C
  .word oIgnore       # 0D
  .word oIgnore       # 0E
  .word oIgnore       # 0F
  .word oIgnore       # 10
  .word oIgnore       # 11
  .word oIgnore       # 12
  .word oIgnore       # 13
  .word oIgnore       # 14
  .word oIgnore       # 15
  .word oIgnore       # 16
  .word oIgnore       # 17
  .word oIgnore       # 18
  .word oIgnore       # 19
  .word oIgnore       # 1A
  .word oIgnore       # 1B
  .word oIgnore       # 1C
  .word oIgnore       # 1D
  .word oIgnore       # 1E
  .word oIgnore       # 1F
  
  
#--------------------------
__oRUN
  lda <OP>; asl A; tax; jmp [[OPCODES+X]]
__oEND
  ldy 0
  ldx <OPStack>; txs
  stz <OP>;
jmp [tRUN]
__oIgnore
jmp [oEND]
# ============================================
# Store → Operand into 16bit Addresses at Operator
#   if the operator runs out of addresses, the last one is incremented by 1
__oStore
  tsx; stx <R2>
  ldx <OPStack>; txs; stx <R3>
  ldx <R2>
  stz <R0>; stz <R0+1>
  ___loop
    inc X
    bpl (noincrement)
    inc Y; bra (continue)
    ___noincrement
    lda [$0100+X]; sta <R0+0>; inc X
    stz <R0+1>
    beq (continue)
    lda [$0100+X]; sta <R0+1>
    ___continue
    
    inc <R3>; bpl (done)
    pla; sta [<R0>+Y]
  bra (loop)
  ___done
jmp [oEND]
# ============================================
# COMPARE OPS
# ============================================
__oEqual
jmp [oEND]
__oLesser
jmp [oEND]
__oGreater
jmp [oEND]
# ============================================
# MATH OPS
# ============================================
# Add → Operator added to Operand
__oAdd
  # setup: 7 cycles
  tsx
  ldy <OPStack>
  clc
  # operation: 19 cycles per byte
  inc Y; beq (done)
  ___loop
    inc X; bmi (zero)
    pla; adc [$0100+Y]; sta [$0100+Y] 
  inc Y; bne (loop)
  ___done
jmp [oEND]
  # zero: 15 cycles per byte
  ___zero
    lda [$0100+Y]; adc 0; sta [$0100+Y]
  inc Y; bne (zero)
jmp [oEND]

# ============================================
# Sub → Operator subtracted from Operand
__oSub
  # setup: 7 cycles
  tsx
  ldy <OPStack>
  sec
  # operation: 21 cycles per byte
  inc Y; beq (done)
  ___loop
    inc X; bmi (zero)
    lda [$0100+Y]; sbc [$0100+X]; sta [$0100+Y]
  inc Y; bne (loop)
  ___done
jmp [oEND]
  # zero: 15 cycles per byte
  ___zero
    lda [$0100+Y]; sbc 0; sta [$0100+Y]
  inc Y; bne (zero)
jmp [oEND]

# ============================================
# And → Operator AND with Operand
__oAnd
  # setup: 5 cycles
  tsx
  ldy <OPStack>
  # operation: 19 cycles per byte
  inc Y; beq (done)
  ___loop
    inc X; bmi (zero)
    pla; and [$0100+Y]; sta [$0100+Y] 
  inc Y; bne (loop)
  ___done
jmp [oEND]
  ___zero
  # zero: 9 cycles per byte
  lda 0
  ____loop  
    sta [$0100+Y]
  inc Y; bne (loop)
jmp [oEND]

# ============================================
# Or → Operator OR with Operand
__oOr
  # setup: 5 cycles
  tsx
  ldy <OPStack>
  # operation: 19 cycles per byte
  inc Y; beq (done)
  ___loop
    inc X; bmi (done)
    pla; ora [$0100+Y]; sta [$0100+Y] 
  inc Y; bne (loop)
  ___done
jmp [oEND]

# ============================================
# Xor → Operator XOR with Operand
__oXor
  # setup: 5 cycles
  tsx
  ldy <OPStack>
  # operation: 19 cycles per byte
  inc Y; beq (done)
  ___loop
    inc X; bmi (done)
    pla; xor [$0100+Y]; sta [$0100+Y] 
  inc Y; bne (loop)
  ___done
jmp [oEND]