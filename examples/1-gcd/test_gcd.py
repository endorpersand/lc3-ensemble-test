import math
from ensemble_test import autograder
from parameterized import parameterized

class GCDTestSuite(autograder.LC3UnitTestCase):
    def setUp(self):
        super().setUp()
        self.loadFile("GCD.asm")

        self.defineSubroutine("MOD", ["a", "b"])
        self.defineSubroutine("GCD", ["a", "b"])

    @parameterized.expand([
        (15, 4),
        (19, 2),
        (17, 7),
        (18, 8)
    ],
        name_func=autograder.parameterized_name
    )
    def test_mod(self, a, b):
        """
        (gcd) MOD Stack & Value
        """
        self.callSubroutine("MOD", [a, b])
        
        self.assertReturned()
        self.assertRegsPreserved()
        self.assertStackCorrect()
        self.assertReturnValue(a % b)
    
    def test_mod_many(self):
        """
        (gcd) MOD Coverage
        """
        for a in range(50):
            for b in range(1, 10):
                self.callSubroutine("MOD", [a, b])
                
                self.assertReturned()
                self.assertRegsPreserved()
                self.assertStackCorrect()
                self.assertReturnValue(a % b)
    
    @parameterized.expand([
        (15, 5),
        (19, 2),
        (17, 7),
        (18, 8)
    ],
        name_func=autograder.parameterized_name
    )
    def test_gcd(self, a, b):
        """
        (gcd) GCD Stack & Value
        """
        self.callSubroutine("GCD", [a, b])
        
        self.assertReturned()
        self.assertRegsPreserved()
        self.assertStackCorrect()
        self.assertReturnValue(math.gcd(a, b))
    
    def test_gcd_many(self):
        """
        (gcd) GCD Coverage
        """
        self.loadFile("GCD.asm")
        self.defineSubroutine("GCD", ["a", "b"])

        for a in range(50):
            for b in range(20):
                self.callSubroutine("GCD", [a, b])
                
                self.assertReturned()
                self.assertRegsPreserved()
                self.assertStackCorrect()
                self.assertReturnValue(math.gcd(a, b))
    
    @parameterized.expand([
        (15, 5),
        (19, 2),
        (17, 7),
        (18, 8)
    ],
        name_func=autograder.parameterized_name
    )
    def test_gcd_calls_mod(self, a, b):
        """
        (gcd) GCD Calls MOD
        """
        self.callSubroutine("GCD", [a, b])
        self.assertSubroutineCalled("MOD")
    
    def test_gcd_chain_calls_mod(self):
        """
        (gcd) GCD Calls MOD a Bunch
        """
        self.callSubroutine("GCD", [34, 21])
        self.assertSubroutinesCalledInOrder([
            ("MOD", [34, 21]),
            ("MOD", [21, 13]),
            ("MOD", [13, 8]),
            ("MOD", [8, 5]),
            ("MOD", [5, 3]),
            ("MOD", [3, 2]),
            ("MOD", [2, 1])
        ])