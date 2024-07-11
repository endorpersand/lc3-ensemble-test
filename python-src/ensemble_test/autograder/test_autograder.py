import unittest

from ensemble_test import core
from . import InternalArgError, LC3UnitTestCase, CallNode

def _subroutine(name: str, inner: str):
    return f"""
    {name}:
        ADD R6, R6, #-1
        ;; push R7
        ADD R6, R6, #-1
        STR R7, R6, #0
        ;; push R5
        ADD R6, R6, #-1
        STR R5, R6, #0
        ;; set R5 to FP
        ADD R5, R6, #1
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

        {inner}
        
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
        ADD R6, R5, #-1
        ;; pop R5
        LDR R5, R6, #0
        ADD R6, R6, #1
        ;; pop R7
        LDR R7, R6, #0
        ADD R6, R6, #1
        RET
    """

class TestLC3Sample(LC3UnitTestCase):
    def test_reg(self):
        self.loadCode("""
            .orig x3000
            AND R0, R0, #0
            ADD R0, R0, #1
            ADD R0, R0, R0
            ADD R0, R0, R0
            ADD R0, R0, R0
            ADD R0, R0, R0
            ADD R0, R0, R0
            HALT
            .end
        """)
        self.runCode()
        self.assertHalted()
        self.assertReg(0, 32)
    
    def test_value(self):
        self.loadCode("""
            .orig x3000
            ONE:   .fill x1111
            TWO:   .fill x2222
            three: .fill x3333
            four:  .fill x4444
            .end
        """)

        self.assertMemValue("ONE",   0x1111)
        self.assertMemValue("two",   0x2222)
        self.assertMemValue("THREE", 0x3333)
        self.assertMemValue("four",  0x4444)
        
        # doesn't exist
        with self.assertRaises(ValueError):
            self.assertMemValue("five", 0x5555)
    
    def test_array(self):
        self.loadCode("""
            .orig x3000
            ARRAY: .fill x1234
                   .fill xCDE5
                   .fill xB0F6
                   .fill xA987
            .end
        """)

        self.assertArray("ARRAY", [0x1234, 0xCDE5, 0xB0F6, 0xA987])

        with self.assertRaises(AssertionError):
            self.assertArray("ARRAY", [0x0000, 0x0000, 0x0000, 0x0000])

    def test_str(self):
        self.loadCode("""
            .orig x3000
            GOOD_STRINGY: 
                .stringz "HELLO!"
            .end
        """)
        self.assertString("GOOD_STRINGY", "HELLO!")

    def test_str_failures(self):
        self.loadCode("""
            .orig x3000
            GOOD_STRINGY: 
                .stringz "HELLO!"
            BAD_STRINGY:
                .stringz "GOODBYE."
            WORSE_STRINGY:
                .fill xF0
                .fill x9F
                .fill x98
                .fill x94
                .fill 0
            .end
        """)
        # non-ascii string test
        with self.assertRaises(InternalArgError) as e:
            self.assertString("BAD_STRINGY", "\U0001F614")

        # non-byte
        with self.assertRaises(AssertionError) as e:
            self.assertString("WORSE_STRINGY", "HELLO")
        self.assertIn("Found invalid ASCII byte", str(e.exception))

        # mismatch test
        with self.assertRaises(AssertionError) as e:
            self.assertString("BAD_STRINGY", "GOODBYE?")
            self.assertIn("did not match expected", str(e.exception))

        # mismatch test
        with self.assertRaises(AssertionError) as e:
            self.assertString("BAD_STRINGY", "GOOBYEEE")
            self.assertIn("did not match expected", str(e.exception))

        # early cut test
        with self.assertRaises(AssertionError) as e:
            self.assertString("BAD_STRINGY", "GOODBYE...?")
            self.assertIn("shorter than expected", str(e.exception))
        
        # late cut test
        with self.assertRaises(AssertionError) as e:
            self.assertString("BAD_STRINGY", "GOOD")
            self.assertIn("longer than expected", str(e.exception))
        

    def test_output(self):
        self.loadCode("""
            .orig x3000
            LD R1, _126
            NOT R1, R1
            ADD R1, R1, #1 ;; R1 = -126
            
            LD R0, _32
            LOOP:
                ADD R2, R0, R1
                BRp ENDLOOP
                PUTC
                ADD R0, R0, #1
            BR LOOP
            ENDLOOP:
            HALT
                           
            _32:  .fill 32
            _126: .fill 126
            .end
        """)
        self.runCode()
        self.assertHalted()
        self.assertOutput(''.join([chr(i) for i in range(32, 127)]))

    def test_pc(self):
        self.loadCode("""
            .orig x3000
                NOP
                NOP
                NOP
                HALT
            .end
        """)

        self.runCode()
        self.assertHalted()
        self.assertPC(0x3003)
    
    def test_cc(self):
        self.loadCode("""
            .orig x3000
                AND R0, R0, #0
                ADD R0, R0, #-1
                HALT
            .end
        """)
        self.runCode()

        # cc failure
        with self.assertRaises(InternalArgError):
            self.assertCondCode("q") # type: ignore
        
        # cc success
        self.assertCondCode("n")
    
    def test_call_sr_standard_cc(self):
        self.loadFile("test-asm/sumtorial-lc3cc.asm")
        

        # assert callSubroutine errors if no defined SR
        with self.assertRaises(InternalArgError) as e:
            self.callSubroutine("SUMTORIAL", [15])
            self.assertIn("No definition provided", str(e.exception))

        # ---------------------------------------------
        self.defineSubroutine("SUMTORIAL", ["n"])

        # assert callSubroutine errors if wrong number of arguments
        with self.assertRaises(InternalArgError) as e:
            self.callSubroutine("SUMTORIAL", [15, 77, 99, 14])
            self.assertIn("Number of arguments provided", str(e.exception))

        # test callSubroutine success
        sumtorial_addr = self._lookup("SUMTORIAL")

        self.assertEqual(
            self.callSubroutine("SUMTORIAL", [3]),
            [
                CallNode(frame_no=1, callee=sumtorial_addr, args=[3], ret=6),
                CallNode(frame_no=2, callee=sumtorial_addr, args=[2], ret=3),
                CallNode(frame_no=3, callee=sumtorial_addr, args=[1], ret=1),
                CallNode(frame_no=4, callee=sumtorial_addr, args=[0], ret=0),
            ],
            "subroutine call did not match expected call stack"
        )

        # ---------------------------------------------
        # suppose we ran it normally:
        for N in range(15):
            self.sim.pc = 0x3000
            self.writeMemValue("N", N)
            self.runCode()
            self.assertHalted()
            self.assertReg(6, 0xD000)
            self.assertReg(0, N * (N + 1) // 2)

    def test_call_sr_pass_by_register(self):
        self.loadFile("test-asm/sumtorial-pbr.asm")

        # ---------------------------------------------
        self.defineSubroutine("SUMTORIAL", {0: "n"}, ret=0)

        # assert callSubroutine errors if wrong number of arguments
        with self.assertRaises(InternalArgError) as e:
            self.callSubroutine("SUMTORIAL", [15, 77, 99, 14])
            self.assertIn("Number of arguments provided", str(e.exception))

        # test callSubroutine success
        sumtorial_addr = self._lookup("SUMTORIAL")

        self.assertEqual(
            self.callSubroutine("SUMTORIAL", [3]),
            [
                CallNode(frame_no=1, callee=sumtorial_addr, args=[3], ret=6),
                CallNode(frame_no=2, callee=sumtorial_addr, args=[2], ret=3),
                CallNode(frame_no=3, callee=sumtorial_addr, args=[1], ret=1),
                CallNode(frame_no=4, callee=sumtorial_addr, args=[0], ret=0),
            ],
            "subroutine call did not match expected call stack"
        )

        # ---------------------------------------------
        # suppose we ran it normally:
        for N in range(15):
            self.sim.pc = 0x3000
            self.writeMemValue("N", N)
            self.runCode()
            self.assertHalted()
            self.assertReg(6, 0xD000)
            self.assertReg(0, N * (N + 1) // 2)
    
    def test_call_sr_standard_cc_other(self):
        self.loadFile("test-asm/double-quad-lc3cc.asm")

        # ---------------------------------------------
        self.defineSubroutine("DOUBLE", ["n"])
        self.defineSubroutine("QUADRUPLE", ["n"])

        # test callSubroutine success
        double_addr = self._lookup("DOUBLE")
        quadruple_addr = self._lookup("QUADRUPLE")

        self.assertEqual(
            self.callSubroutine("QUADRUPLE", [3]),
            [
                CallNode(frame_no=1, callee=quadruple_addr, args=[3], ret=12),
                CallNode(frame_no=2, callee=double_addr, args=[3], ret=6),
                CallNode(frame_no=2, callee=double_addr, args=[6], ret=12),
            ],
            "subroutine call did not match expected call stack"
        )

        # ---------------------------------------------
        # suppose we ran it normally:
        for N in range(15):
            self.sim.pc = 0x3000
            self.writeMemValue("N", N)
            self.runCode()
            self.assertHalted()
            self.assertReg(6, 0xD000)
            self.assertReg(0, 4 * N)

    # def test_stack_frame(self):
    #     self.loadFile("test-asm/xy-lc3cc.asm")

    #     self.defineSubroutine("X", ["n"])
    #     self.defineSubroutine("Y", ["n"])
    #     try:
    #         self.runCode()
    #     except core.SimError as e:
    #         self._printStackFrame()
    #         self.fail(e)
    #     self.assertReg(0, 2)
    
    def test_halt(self):
        # halting program
        self.loadCode("""
            .orig x3000
            AND R0, R0, #0
            ADD R0, R0, #15
            LOOP: 
                BRnz END
                ADD R0, R0, #-1
                BR LOOP
            END: 
                HALT
            .end
        """)
        self.runCode()
        self.assertHalted()

        # infinite loop
        self.loadCode("""
            .orig x3000
            THIS BR THIS
            .end
        """)
        self.runCode()
        with self.assertRaises(AssertionError) as e:
            self.assertHalted()
            self.assertIn("halt", str(e.exception))

    def test_regs_preserved(self):
        self.loadCode("""
            .orig x3000
            LD R6, SP
            JSR SR
            HALT

            SR
                ADD R6, R6, #-1
                STR R0, R6, #0
                      
                AND R0, R0, #0
                ADD R0, R0, #2
                      
                LDR R0, R6, #0
                ADD R6, R6, #1
                RET
            
            SP .fill xF000
            .end
        """)

        self.runCode()
        self.assertRegsPreserved([0, 1, 2, 3, 4, 5])

        self.loadCode(f"""
            .orig x3000
                {_subroutine("SR", '''
                    AND R0, R0, #0
                    ADD R0, R0, #0
                ''')}
            .end
        """)
        self.defineSubroutine("SR", [])
        self.callSubroutine("SR", [])
        self.assertRegsPreserved([0, 1, 2, 3, 4, 5])

    def test_stack_correct(self):
        self.loadCode(f"""
            .orig x3000
            {_subroutine("SR", '''
                AND R0, R0, #0
            ''')}
            .end
        """)

        self.defineSubroutine("SR", [])
        self.callSubroutine("SR", [])

        self.assertStackCorrect()

    def test_exec_asserts(self):
        self.loadCode(f"""
            .orig x3000
                {_subroutine("SR", "")}
            .end
        """)

        ## NO EXECUTION CALLS
        with self.assertRaises(InternalArgError):
            self.assertRegsPreserved()
        with self.assertRaises(InternalArgError):
            self.assertHalted()
        with self.assertRaises(InternalArgError):
            self.assertReturned()
        with self.assertRaises(InternalArgError):
            self.assertStackCorrect()
        
        ## CALL SUBROUTINE
        self.defineSubroutine("SR", [])
        self.callSubroutine("SR", [])

        self.assertRegsPreserved([0, 1, 2, 3, 4, 5])
        with self.assertRaises(InternalArgError):
            self.assertHalted()
        self.assertReturned()
        self.assertStackCorrect()

        ## RUN CODE
        self.loadCode("""
            .orig x3000
                HALT
            .end
        """)
        self.runCode()

        self.assertRegsPreserved([0, 1, 2, 3, 4, 5, 6, 7])
        self.assertHalted()
        with self.assertRaises(InternalArgError):
            self.assertReturned()
        with self.assertRaises(InternalArgError):
            self.assertStackCorrect()

    def test_exec_asserts_fail(self):
        ## CALL SUBROUTINE
        self.loadCode(f"""
            .orig x3000
                SR: 
                    AND R0, R0, #0
                    RET
            .end
        """)
        
        self.defineSubroutine("SR", [])
        self.callSubroutine("SR", [])

        with self.assertRaises(AssertionError) as e:
            self.assertRegsPreserved([0, 1, 2, 3, 4, 5])
            self.assertIn("register 0", str(e.msg))
        
        self.loadCode(f"""
            .orig x3000
                SR: BR SR
            .end
        """)
        self.defineSubroutine("SR", [])
        self.callSubroutine("SR", [])
        with self.assertRaises(AssertionError):
            self.assertReturned()
        with self.assertRaises(AssertionError):
            self.assertStackCorrect()

        ## RUN CODE
        self.loadCode("""
            .orig x3000
                LOOP: BR LOOP
            .end
        """)
        self.runCode()

        self.assertRegsPreserved([0, 1, 2, 3, 4, 5, 6, 7])
        with self.assertRaises(AssertionError):
            self.assertHalted()

if __name__ == "__main__":
    unittest.main()