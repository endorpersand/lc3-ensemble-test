# These imports pretty much always have to be imported,
# because they'll be used in almost all LC3 autograders.
from ensemble_test import autograder
from parameterized import parameterized

class UppercaseTestSuite(autograder.LC3UnitTestCase):
    def setUp(self):
        super().setUp()

        # Required initialization step.
        # Must be either self.loadFile or self.loadCode.
        #
        # Note for loadFile, the path is relative to the directory where THIS TEST CASE resides.
        self.loadFile("student.asm")

        # If using callSubroutine, these have to be defined:
        self.defineSubroutine("TO_UPPERCASE1", ["char"])
        self.defineSubroutine("TO_UPPERCASE2", { 2: "char" }, ret=3)
    
    # Example of a program execution test case.
    #
    # For this parameterized.expand decorator,
    # - the first argument holds all of the possible cases to test,
    # - the name_func describes the naming format (pretty much should always be autograder.parameterized_name), and
    # - the doc_func describes the documentation format (here, we use autograder.parameterized_doc).
    @parameterized.expand([
        "m", "i", "n", "e", "c", "r", "a", "f", "t"
    ],
        name_func=autograder.parameterized_name,
        doc_func=autograder.parameterized_doc
    )
    def test_program(self, char: str):
        """
        (to_uppercase) Program Uppercase {0!r}
        """

        # Most reads and writes belong before the execution.
        # Write parameter to given location:
        self.writeMemValue("CHAR", ord(char))

        # Each test case must execute something.
        # Here, we execute the entire program.
        self.runCode()
        self.assertHalted()

        # Assert values
        self.assertReg(0, ord(char.upper()))
        self.assertReg(3, ord(char.upper()))
    
    # Example of a standard calling convention subroutine execution test case.
    #
    # For this parameterized.expand decorator,
    # - we will use a range to generalize the first argument,
    # - we will use default doc_func to see what that looks like
    @parameterized.expand(
        ["m", "i", "n", "e", "c", "r", "a", "f", "t"],
        name_func=autograder.parameterized_name
    )
    def test_standard_cc(self, char: str):
        """
        (to_uppercase) Standard Call TO_UPPERCASE
        """

        # Here, we're executing a subroutine.
        self.callSubroutine("TO_UPPERCASE1", [ord(char)])
        self.assertReturned()
        self.assertRegsPreserved()
        self.assertStackCorrect()

        # Assert values
        self.assertReturnValue(ord(char.upper()))
    
    # Example of a pass-by-register calling convention execution test case.
    #
    # This is pretty similar to the `test_standard_cc` test.
    @parameterized.expand(
        ["m", "i", "n", "e", "c", "r", "a", "f", "t"],
        name_func=autograder.parameterized_name
    )
    def test_pass_by_register_cc(self, char: str):
        """
        (to_uppercase) Pass-by-register Call TO_UPPERCASE
        """
        # Also executing a subroutine.
        # Syntax stays the same across different calling conventions.
        self.callSubroutine("TO_UPPERCASE2", [ord(char)])
        self.assertReturned()
        self.assertRegsPreserved([0, 1, 2, 4, 5])
        self.assertStackCorrect() # practically just self.assertRegsPreserved([6]) on pass-by-register

        # Assert values
        self.assertReturnValue(ord(char.upper()))
    
    # Coverage test
    def test_standard_cc_coverage(self):
        """
        (to_uppercase) Coverage Test
        """
        for c in range(32, 127):
            self.loadFile("student.asm")
            self.defineSubroutine("TO_UPPERCASE1", ["char"])
            self.defineSubroutine("TO_UPPERCASE2", { 2: "char" }, ret=3)

            self.callSubroutine("TO_UPPERCASE1", [c])
            
            self.assertReturned()
            self.assertRegsPreserved()
            self.assertStackCorrect()
            self.assertReturnValue(ord(chr(c).upper()))