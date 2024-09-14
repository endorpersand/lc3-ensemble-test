from ensemble_test import autograder
from parameterized import parameterized

class IOTestSuite(autograder.LC3UnitTestCase):
    def setUp(self):
        super().setUp()
        self.loadFile("io.asm")

    @parameterized.expand([
        "I'd just like to interject for a moment.",
        "What you're refering to as Linux, is in fact, GNU/Linux,",
        "or as I've recently taken to calling it, GNU plus Linux.",
        "Linux is not an operating system unto itself, but rather",
        "another free component of a fully functioning GNU system",
        "made useful by the GNU corelibs, shell utilities and vital",
        "system components comprising a full OS as defined by POSIX."
    ])
    def test_io(self, text: str):
        self.setInput(text + "\n")
        
        self.runCode()
        self.assertHalted()
        
        self.assertOutput("\n".join(text.split(" ")) + "\n")