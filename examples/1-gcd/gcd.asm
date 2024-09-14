;;=============================================================
;;  MOD & GCD
;;=============================================================

.orig x3000
    LD R6, STACK_PTR

    ;; Pushes arguments A and B
    ADD R6, R6, -2
    LD R1, A
    STR R1, R6, 0
    LD R1, B
    STR R1, R6, 1 
    JSR GCD
    LDR R0, R6, 0
    ADD R6, R6, 3
    HALT

    STACK_PTR   .fill xF000
    A           .fill 10
    B           .fill 4


;;  MOD(int a, int b) {
;;      while (a >= b) {
;;          a -= b;
;;      }
;;      return a;
;;  }

MOD:
    ADD R6, R6, -4
    
    STR R5, R6, 1
    ADD R5, R6, 0

    STR R7, R6, 2

    ADD R6, R6, -5
    STR R4, R6, 0
    STR R3, R6, 1
    STR R2, R6, 2
    STR R1, R6, 3
    STR R0, R6, 4
    
    LDR R1, R5, 4
    LDR R2, R5, 5
    
    NOT R3, R2
    ADD R3, R3, 1
    
    MOD_WHILE
    ADD R4, R1, R3
    BRn MOD_RETURN
    
    ADD R1, R1, R3
    BR MOD_WHILE

    MOD_RETURN
    STR R1, R5, 3
    LDR R4, R6, 0
    LDR R3, R6, 1
    LDR R2, R6, 2
    LDR R1, R6, 3
    LDR R0, R6, 4

    ADD R6, R6, 8
    LDR R5, R6, -2
    LDR R7, R6, -1
    RET

;;  GCD(int a, int b) {
;;      if (b == 0) {
;;          return a;
;;      }
;;        
;;      while (b != 0) {
;;          int temp = b;
;;          b = MOD(a, b);
;;          a = temp;
;;      }
;;      return a;
;;  }

GCD:
    ADD R6, R6, -4
    
    STR R5, R6, 1
    ADD R5, R6, 0

    STR R7, R6, 2

    ADD R6, R6, -5
    STR R4, R6, 0
    STR R3, R6, 1
    STR R2, R6, 2
    STR R1, R6, 3
    STR R0, R6, 4
    
    LDR R1, R5, 4
    LDR R2, R5, 5

    BRz GCD_RETURN

    GCD_WHILE
    ADD R3, R2, 0
    BRz GCD_RETURN
    
    ADD R6, R6, -2
    STR R1, R6, 0
    STR R2, R6, 1
    JSR MOD
    LDR R2, R6, 0
    ADD R1, R3, 0
    ADD R6, R6, 3
    BR GCD_WHILE
    
    GCD_RETURN
    STR R1, R5, 3
    
    LDR R4, R6, 0
    LDR R3, R6, 1
    LDR R2, R6, 2
    LDR R1, R6, 3
    LDR R0, R6, 4

    ADD R6, R6, 8
    LDR R5, R6, -2
    LDR R7, R6, -1
    RET
.end