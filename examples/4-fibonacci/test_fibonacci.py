from ensemble_test import autograder
from parameterized import parameterized

def fibonacci(n: int):
    if n < 2: return n
    return fibonacci(n - 1) + fibonacci(n - 2)

class FibonacciTestSuite(autograder.LC3UnitTestCase):
    def setUp(self):
        super().setUp()

        self.loadFile("fibonacci.asm")
        self.defineSubroutine("FIBONACCI", ["n"])

    @parameterized.expand([
        1, 2, 3, 4, 5, 6, 7, 8, 9, 10
    ])
    def test_fibonacci(self, n: int):
        """
        (subslice) Fibonacci Value
        """
        self.callSubroutine("FIBONACCI", [n])
        
        self.assertReturned()
        self.assertRegsPreserved()
        self.assertStackCorrect()

        self.assertReturnValue(fibonacci(n))
    
    @parameterized.expand([
        2, 3, 4, 5, 6, 7, 8, 9, 10
    ])
    def test_fibonacci_recurses(self, n: int):
        """
        (subslice) Fibonacci Recurses
        """
        self.callSubroutine("FIBONACCI", [n])
        
        self.assertReturned()
        self.assertRegsPreserved()
        self.assertStackCorrect()

        self.assertSubroutineCalled("FIBONACCI", [n - 1])
        self.assertSubroutineCalled("FIBONACCI", [n - 2])