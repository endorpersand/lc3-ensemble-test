;;=============================================================
;;  SPLIT_WORD() {
;;    input = GETC;
;;    if (input == '\n') {
;;      PUTC;
;;      break;
;;    }
;;    if (input == ' ') {
;;      R0 = '\n';
;;    }
;;    PUTC;
;;  }
;;=============================================================

.orig x3000
    LOOP:
        GETC

        LD R1, NL
        NOT R1, R1
        ADD R1, R1, #1
        ADD R1, R1, R0
        BRnp ELSE_NOT_NL
        PUTC
        BR END


        ELSE_NOT_NL:
        LD R1, SPACE
        NOT R1, R1
        ADD R1, R1, #1
        ADD R1, R1, R0
        BRnp ELSE_NOT_SPACE
        LD R0, NL

        ELSE_NOT_SPACE:
        PUTC
        BR LOOP
    END:
        HALT

    NL: .fill xA
    SPACE: .fill x20
.end