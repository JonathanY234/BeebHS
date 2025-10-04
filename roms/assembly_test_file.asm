; 6502 Assembly - Compute 10th Fibonacci number
; Stores result in FIB1

        LDX #10        ; Number of iterations
        LDA #0
        STA $00        ; FIB0
        LDA #1
        STA $01        ; FIB1

LOOP:
        LDA $00        ; A = FIB0
        CLC
        ADC $01        ; A = FIB0 + FIB1
        STA $02        ; TEMP

        LDA $01        ; shift FIB1 -> FIB0
        STA $00
        LDA $02        ; TEMP -> FIB1
        STA $01

        DEX
        BNE LOOP

        BRK            ; program end

; Zero page variables:
; $00 = FIB0
; $01 = FIB1
; $02 = TEMP