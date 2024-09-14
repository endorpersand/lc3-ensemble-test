;;=============================================================
;;  FIBONACCI(int n) {
;;    if (n == 0) return 0;
;;    if (n == 1) return 1;
;;    left = FIBONACCI(n - 2);
;;    right = FIBONACCI(n - 1);
;;    return left + right;
;;  }
;;=============================================================

.orig x3000
    LD R6, STACK_PTR

    ;; Push argument
    ADD R6, R6, #-1
    LD R1, N
    STR R1, R6, #0
    JSR FIBONACCI
    LDR R0, R6, #0
    ADD R6, R6, #2
    HALT

    STACK_PTR .fill xF000
    N .fill 7

    FIBONACCI:
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

        LDR R0, R5, #4
        ;; if (n < 2) return n;
        ;; if (2 - n > 0) return n;
        NOT R1, R0
        ADD R1, R1, #1
        ADD R1, R1, #2
        BRp END
        
        ;; R1 <- FIBONACCI(n - 1)
        ADD R0, R0, #-1
        ADD R6, R6, #-1
        STR R0, R6, #0
        JSR FIBONACCI
        LDR R1, R6, #0
        ADD R6, R6, #2

        ;; R0 <- FIBONACCI(n - 2)
        ADD R0, R0, #-1
        ADD R6, R6, #-1
        STR R0, R6, #0
        JSR FIBONACCI
        LDR R0, R6, #0
        ADD R6, R6, #2

        ADD R0, R1, R0
        
        END:
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
.end

.orig x4000
    SRC .blkw 100
.end
.orig x5000
    DEST .blkw 100
.end