from ensemble_test import autograder
from parameterized import parameterized

class SubsliceTestSuite(autograder.LC3UnitTestCase):
    def setUp(self):
        super().setUp()

        self.loadFile("subslice.asm")
        self.defineSubroutine("SUBSLICE", ["dest", "src", "start", "end"])

    @parameterized.expand([
        ("abcdefghij", 1, 4),
        ("hello!", 1, 4),
        ("resnmtsrtnmrs", 3, 7),
        ("welcome!", 0, 11),
        ("dckvdncvkcden", -1, 3),
        ("surtyurstrs", 4, 100),
        ("empty", 9, 11)
    ])
    def test_subslice(self, text: str, start: int, end: int):
        """
        (subslice) Subslice Test
        """
        src_addr = self.readMemValue("SRC_ADDR")
        dest_addr = self.readMemValue("DEST_ADDR")

        self.writeString(src_addr, text)
        self.callSubroutine("SUBSLICE", [dest_addr, src_addr, start, end])

        self.assertReturned()
        self.assertRegsPreserved()
        self.assertStackCorrect()

        start = max(0, start)
        end = min(len(text), end)
        self.assertString(dest_addr, text[start:end])
