"""
Test case utility.
"""
import itertools
import typing
from .. import core
import unittest

__unittest = True
"""
Flag to hide traceback from this file in unit tests.

Useful for removing the tracebacks from helper functions here! :D
"""

def _to_u16(val: int) -> int:
    return val & 0xFFFF
def _to_i16(val: int) -> int:
    val = _to_u16(val)
    return val - (val >> 15) * 65536
def _require_ascii_string(string: str, *, arg_desc: str | None = None) -> bytes:
    try:
        return string.encode("ascii")
    except UnicodeEncodeError:
        arg_desc = arg_desc or f"string parameter ({string=!r})"
        raise ValueError(f"{arg_desc} should be an ASCII string")
    
class LC3UnitTestCase(unittest.TestCase):
    def setUp(self):
        self.sim = core.Simulator()
        self.seed = self.sim.init(core.MemoryFillType.Random)
        self.longMessage = False
    
    def _readContiguous(self, start_addr: int, length: int | None = None):
        ctr = itertools.count() if length is None else range(length)
        return (self.sim.read_mem(_to_u16(start_addr + i)) for i in ctr)
    
    def _lookup(self, label: str) -> int:
        addr = self.sim.lookup(label)
        if addr is None:
            raise ValueError(f"Label {label.upper()} is missing in the assembly code")

        return addr
    
    def _simpleAssertMsg(self, msg, expected, actual):
        return (
            f"{msg}\n"
            f"expected: {expected}\n"
            f"actual:   {actual}"
        )
        
    def _assertShortEqual(self, expected: int, actual: int, msg_fmt: str | None = None, *, signed: bool = True, show_hex: bool = True):
        expected_i, expected_u = _to_i16(expected), _to_u16(expected)
        actual_i, actual_u = _to_i16(actual), _to_u16(actual)

        msg = (msg_fmt or "Expected {expected}, but got {actual}").format(
            expected=f"{expected_i if signed else expected_u}" + f" (x{expected_u:04X})" if show_hex else "",
            actual=f"{actual_i if signed else actual_u}" + f" (x{actual_u:04X})" if show_hex else ""
        )

        self.assertEqual(expected_u, actual_u, msg)
    
    def writeMemValue(self, label: str, value: int):
        addr = self._lookup(label)
        self.sim.write_mem(addr, _to_u16(value))
    
    def writeArray(self, label: str, lst: list[int]):
        addr = self._lookup(label)
        
        for i, e in enumerate(lst):
            self.sim.write_mem(addr + i, _to_u16(e))

    def writeString(self, label: str, string: str):
        addr = self._lookup(label)
        string_bytes = _require_ascii_string(string, arg_desc=f"string value parameter ({string=!r})")

        for i, byte in enumerate(string_bytes):
            self.sim.write_mem(addr + i, _to_u16(byte))
        self.sim.write_mem(addr + len(string_bytes), 0)

    def setInput(self, inp: str):
        _require_ascii_string(inp, arg_desc=f"input parameter ({inp=!r})")
        self.sim.input = inp

    def assertReg(self, reg_no: int, expected: int):
        actual = self.sim.get_reg(reg_no)

        self._assertShortEqual(expected, actual, 
            f"Expected register {reg_no} to be {{expected}}, but it was {{actual}}")
    
    def assertMemValue(self, label: str, expected: int):
        addr = self._lookup(label)
        actual = self.sim.read_mem(addr)

        self._assertShortEqual(expected, actual, 
            f"Expected mem[{label.upper()}] to be {{expected}}, but it was {{actual}}")

    def assertArray(self, label: str, arr: list[int]):
        addr = self._lookup(label)
        
        expected = [_to_u16(e) for e in arr]
        actual = list(self._readContiguous(addr, len(arr)))

        self.assertEqual(expected, actual, 
            self._simpleAssertMsg(f"Array at label {label.upper()} did not match expected", expected, actual))

    def assertString(self, label: str, expected_str: str):
        addr = self._lookup(label)
        expected_bytes = _require_ascii_string(expected_str, arg_desc=f"expected string parameter ({expected_str=!r})")
        
        expected = [*expected_bytes, 0]
        actual = list(self._readContiguous(addr, len(expected)))
        
        try:
            nul_pos = actual.index(0)
        except ValueError:
            nul_pos = -1
        
        # if nul_pos is -1, this gets everything except the last byte. we print the actual_str as "XYZ...", so this is what we want
        # if nul_pos is not -1, this gets everything right before the null-terminator, which is what we want
        for e in actual[:nul_pos]:
            self.assertTrue(0 <= e <= 127, f"Found invalid ASCII byte in string at label {label.upper()}: {actual}")
        actual_str = bytes(actual[:nul_pos]).decode("ascii")

        for e, a in zip(expected, actual):
            if e == 0 and a == 0:
                break

            if e != 0 and a == 0:
                self.fail(
                    self._simpleAssertMsg(f"String at {label.upper()} shorter than expected",
                        f"{expected_str} {expected}",
                        f"{actual_str.ljust(len(expected_str))} {actual}")
                )

            if e == 0 and a != 0:
                self.fail(
                    self._simpleAssertMsg(f"String at {label.upper()} longer than expected", 
                        f"{expected_str}    {expected}", 
                        f"{actual_str}... {actual}")
                )
            
            if e != 0 and a != 0:
                pad = max(len(expected_str), len(actual_str))

                self.assertEqual(e, a,
                    self._simpleAssertMsg(f"String at label {label.upper()} did not match expected", 
                        f"{expected_str.ljust(pad)} {expected}", 
                        f"{actual_str.ljust(pad)} {actual}"
                    )
                )
    
    def assertConsoleOutput(self, expected: str):
        # There's technically nothing wrong with non-ASCII inputs for this method,
        # and it could accept non-ASCII text if it wanted

        # But just for consistency and too-lazy-to-verify-correctness,
        # we'll just require it's ASCII
        _require_ascii_string(expected, arg_desc=f"expected string parameter ({expected=!r})")
        actual = self.sim.output
        self.assertEqual(expected, actual,
                         self._simpleAssertMsg("Console output did not match expected", expected, actual))

    def assertPC(self, expected: int):
        self._assertShortEqual(expected, self.sim.pc,
                                  f"Expected PC to be {{expected}}, but it was {{actual}}",
                                  signed = False)
    
    def assertCondCode(self, expected: typing.Literal["n", "z", "p"]):
        if expected not in ('n', 'z', 'p'): raise ValueError(f"expected parameter should be 'n', 'z', or 'p' ({expected=!r})")
        n, z, p = self.sim.n, self.sim.z, self.sim.p

        if n + z + p < 1:
            raise RuntimeError("Simulation error: None of the condition codes are enabled")
        if n + z + p > 1:
            raise RuntimeError(f"Simulation error: More than 1 condition code is enabled ({n=}, {z=}, {p=})")

        if n:
            actual = "n"
        elif z:
            actual = "z"
        else:
            actual = "p"

        self.assertEqual(expected, actual, f"Expected condition code to be {expected}, but it was {actual}")