;;=============================================================
;;  TO_UPPERCASE(char c) {
;;    if (c < 'a') {
;;      return c;
;;    }
;;    if (c > 'z') {
;;      return c;
;;    }
;;  
;;    return c & xDF;
;;  }
;;  
;;=============================================================

.orig x3000
    LD R6, STACK_PTR

    ;; Standard LC3 calling convention
    ;; call TO_UPPERCASE(CHAR)
    ADD R6, R6, #-1
    LD R1, CHAR
    STR R1, R6, #0
    JSR TO_UPPERCASE1
    LDR R0, R6, #0
    ADD R6, R6, #2

    ;; Pass-by-register calling convention
    ;; call TO_UPPERCASE(R2) -> R3
    LD R2, CHAR
    JSR TO_UPPERCASE2
    HALT

    ;; can change argument CHAR
    CHAR:      .fill x69
    STACK_PTR: .fill xF000

TO_UPPERCASE1:
    ADD R6, R6, #-1
    ;; push R7
    ADD R6, R6, #-1
    STR R7, R6, #0
    ;; push R5
    ADD R6, R6, #-1
    STR R5, R6, #0
    ;; set R5 to FP
    ADD R5, R6, #-1
    ;; push R0-R4
    ADD R6, R6, #-1
    STR R0, R6, #0
    ADD R6, R6, #-1
    STR R1, R6, #0
    ADD R6, R6, #-1
    STR R2, R6, #0
    ADD R6, R6, #-1
    STR R3, R6, #0
    ADD R6, R6, #-1
    STR R4, R6, #0

    ;; R0 = c
    ;; R1 = -c
    LDR R0, R5, #4
    
    ADD R1, R0, #0
    NOT R1, R1
    ADD R1, R1, #1

    ;; R2 = 'a' - c
    ;; if (R2 > 0) return c
    LD R2, MIN_CHAR
    ADD R2, R2, R1
    BRp END1
    
    ;; R2 = 'z' - c
    ;; if (R2 < 0) return c
    LD R2, MAX_CHAR
    ADD R2, R2, R1
    BRn END1

    ;; c = c & 0x20
    LD R2, MASK
    AND R0, R0, R2

    END1:
    STR R0, R5, #3

    ;; pop R0-R4
    LDR R4, R6, #0
    ADD R6, R6, #1
    LDR R3, R6, #0
    ADD R6, R6, #1
    LDR R2, R6, #0
    ADD R6, R6, #1
    LDR R1, R6, #0
    ADD R6, R6, #1
    LDR R0, R6, #0
    ADD R6, R6, #1
    ;; pop local variables
    ADD R6, R5, #1
    ;; pop R5
    LDR R5, R6, #0
    ADD R6, R6, #1
    ;; pop R7
    LDR R7, R6, #0
    ADD R6, R6, #1
    RET

    MIN_CHAR .fill x61
    MAX_CHAR .fill x7A
    MASK .fill xDF

;; assume TO_UPPERCASE2(R2) -> R3:
TO_UPPERCASE2:
    ST R0, SAVE_R0
    ST R1, SAVE_R1
    ST R2, SAVE_R2

    ;; R2 = c
    ;; R1 = -c
    ADD R1, R2, #0
    NOT R1, R1
    ADD R1, R1, #1

    ;; R0 = 'a' - c
    ;; if (R0 > 0) return c
    LD R0, MIN_CHAR
    ADD R0, R0, R1
    BRp END2
    
    ;; R0 = 'z' - c
    ;; if (R0 < 0) return c
    LD R0, MAX_CHAR
    ADD R0, R0, R1
    BRn END2

    ;; c = c & 0x20
    LD R0, MASK
    AND R2, R2, R0

    END2:
    ADD R3, R2, #0

    LD R0, SAVE_R0
    LD R1, SAVE_R1
    LD R2, SAVE_R2
    RET

    SAVE_R0 .blkw 1
    SAVE_R1 .blkw 1
    SAVE_R2 .blkw 1
.end