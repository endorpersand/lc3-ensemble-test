;;=============================================================
;;  SUBSLICE(char *dest, char *src, int start, int end) {
;;    len = 0
;;    ptr = src
;;    while (*ptr != 0) {
;;      len++;
;;      ptr++;
;;    }
;;
;;    if (start < 0) start = 0;
;;    if (start > len) start = len;
;;    if (end > len) end = len;
;;  
;;    i = 0
;;    while (start + i < end) {
;;      dest[i] = src[start + i]
;;      i++;
;;    }
;;    dest[end] = 0;
;;  }
;;=============================================================

.orig x3000
    LD R6, STACK_PTR

    ;; Push arguments
    ADD R6, R6, #-4
    LD R1, DEST_ADDR
    STR R1, R6, #0
    LD R1, SRC_ADDR
    STR R1, R6, #1
    LD R1, START
    STR R1, R6, #2
    LD R1, END
    STR R1, R6, #3
    JSR SUBSLICE
    ADD R6, R6, #5
    HALT

    STACK_PTR .fill xF000
    SRC_ADDR  .fill SRC
    DEST_ADDR .fill DEST
    START     .fill 0
    END       .fill 10

    SUBSLICE:
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

        ;; R0 = len
        ;; R1 = ptr
        AND R0, R0, #0
        LDR R1, R5, #5
        GET_LEN_LOOP: 
            LDR R2, R1, #0
            BRz END_GET_LEN_LOOP
            ADD R0, R0, #1
            ADD R1, R1, #1
            BR GET_LEN_LOOP
        END_GET_LEN_LOOP
        
        ;; if (start < 0) start = 0;
        LDR R2, R5, #6
        BRzp SKIP_CAP_START_MIN
        AND R2, R2, #0
        STR R2, R5, #6
        SKIP_CAP_START_MIN

        NOT R1, R0
        ADD R1, R1, #1
        ;; if (start > len) start = len;
        LDR R2, R5, #6
        ADD R2, R2, R1
        BRn SKIP_CAP_START_MAX
        STR R0, R5, #6
        SKIP_CAP_START_MAX

        ;; if (end > len) end = len;
        LDR R2, R5, #7
        ADD R2, R2, R1
        BRn SKIP_CAP_END
        STR R0, R5, #7
        SKIP_CAP_END

        ;; R0 = i
        LDR R0, R5, #6
        ;; R1 = dest + i
        LDR R1, R5, #4
        ;; R2 = src + start + i
        LDR R2, R5, #5
        ADD R2, R2, R0

        ;; R3 = -end
        LDR R3, R5, #7
        NOT R3, R3
        ADD R3, R3, #1

        COPY_LOOP:
            ;; while (i - end < 0)
            ADD R4, R0, R3
            BRzp END_COPY_LOOP
            ;; *dest = *src
            LDR R4, R2, #0
            STR R4, R1, #0

            ADD R0, R0, #1
            ADD R1, R1, #1
            ADD R2, R2, #1
            BR COPY_LOOP
        END_COPY_LOOP
        
        AND R4, R4, #0
        STR R4, R1, #0

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