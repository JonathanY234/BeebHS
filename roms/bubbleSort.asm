; ----------------------------------------
; Constants / Variables
; ----------------------------------------
ARRAY   = $0200       ; Start of array in zero page
LEN     = 10          ; Number of elements
limit   = $00FF       ; Temporary variable for loop limit

; ----------------------------------------
; Program Start
; ----------------------------------------
Start:
        LDX #0

; Initialize array with unsorted data
InitArray:
        LDA UnsortedValues X
        STA ARRAY, X
        INX
        CPX #LEN
        BNE InitArray

; ----------------------------------------
; Bubble Sort Routine
; ----------------------------------------
BubbleSort:
        LDX #0              ; X = outer loop counter

OuterLoop:
        LDA #LEN
        SEC
        SBC #1
        SBC X
        STA limit           ; limit = LEN - 1 - X

        LDY #0              ; Y = inner loop counter

InnerLoop:
        LDA ARRAY, Y
        CMP ARRAY+1, Y      ; Compare A with next element
        BCC NoSwap          ; If A < next, skip swap

        ; Swap ARRAY[Y] and ARRAY[Y+1]
        LDA ARRAY, Y
        TAX
        LDA ARRAY+1, Y
        STA ARRAY, Y
        TXA
        STA ARRAY+1, Y

NoSwap:
        INY
        LDA limit
        CMP Y
        BNE InnerLoop       ; Keep looping until Y == limit

        INX
        CPX #LEN
        BNE OuterLoop       ; Keep looping until X == LEN

        BRK                 ; Done (use BRK to stop execution)

; ----------------------------------------
; Data Section
; ----------------------------------------
UnsortedValues:
        .byte 9, 3, 7, 1, 8, 2, 6, 4, 5, 0