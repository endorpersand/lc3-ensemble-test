"""
Test case utility.
"""
from collections.abc import Iterable
import dataclasses
import enum
import itertools
from pathlib import Path
import typing
from .. import core
import unittest

__unittest = True
"""
Flag to hide traceback from this file in unit tests.

Useful for removing the tracebacks from helper functions here! :D
"""

INSTRUCTION_RUN_LIMIT = 0xABCDE

def _to_u16(val: int) -> int:
    return val & 0xFFFF
def _to_i16(val: int) -> int:
    val = _to_u16(val)
    return val - (val >> 15) * 65536

class InternalError(Exception):
    def __str__(self):
        return f"{super().__str__()}\n[This error should not occur. If you see this on Gradescope, contact a TA.]"

def _verify_ascii_string(string: str, *, arg_desc: str | None = None) -> bytes:
    try:
        bstring = string.encode("ascii")
    except UnicodeEncodeError:
        arg_desc = arg_desc or f"string parameter ({string=!r})"
        raise InternalError(f"{arg_desc} should be an ASCII string")

    if any(b == 0 for b in bstring):
        raise InternalError(f"{arg_desc} contains a null terminator")

    return bstring

def _verify_reg_no(reg_no: int):
    if not (0 <= reg_no <= 7):
        raise InternalError(f"cannot access non-existent register {reg_no}")

@dataclasses.dataclass
class CallNode:
    frame_no: int
    callee: int
    args: list[int]
    ret: int | None = None
    
class LC3UnitTestCase(unittest.TestCase):
    def setUp(self):
        self.sim = core.Simulator()
        self.saved_registers: list[int] | None = None

        # Preconditions
        self.init_fill = core.MemoryFillType.Random
        self.init_seed = self.sim.init(self.init_fill)
        self.source_code: Path | str | None = None
        """
        Where the source comes from.
        If this is a path, it comes from some local file.
        If this is a string, it comes from inline code.
        If this is None, it has not yet been declared.
        """
    
    def _readContiguous(self, start_addr: int, length: int | None = None):
        ctr = itertools.count() if length is None else range(length)
        return (self.sim.read_mem(_to_u16(start_addr + i)) for i in ctr)
    
    def _writeContiguous(self, start_addr: int, words: Iterable[int]):
        for i, word in enumerate(words):
            self.sim.write_mem(start_addr + i, _to_u16(word))

    def _lookup(self, label: str) -> int:
        addr = self.sim.lookup(label)
        if addr is None:
            raise InternalError(f"Label {label.upper()} is missing in the assembly code")

        return addr
    
    def _simpleAssertMsg(self, msg, expected, actual):
        return (
            f"{msg}\n"
            f"expected: {expected}\n"
            f"actual:   {actual}"
        )
        
    def _printStackFrame(self):
        stack = self.sim.frames or []
        for i, frame in enumerate(stack):
            name = self.sim.reverse_lookup(frame.callee_addr) or f"x{frame.callee_addr:04X}"
            defn = self.sim.get_subroutine_def(frame.callee_addr)
            fp = frame.frame_ptr and frame.frame_ptr[0]
            r7 = self.sim.get_mem(fp + 2) if fp is not None else None

            fp_str = f"x{fp:04X}" if fp is not None else "?"
            r7_str = f"x{r7:04X}" if r7 is not None else "?"

            if defn is not None:
                if defn[0] == core.SubroutineType.CallingConvention:
                    args = ', '.join(f"{p}={a}" for p, (a, _) in zip(defn[1], frame.arguments))
                elif defn[0] == core.SubroutineType.PassByRegister:
                    args = ', '.join(f"{p}={a}" for (p, _), (a, _) in zip(defn[1], frame.arguments))
                else:
                    raise NotImplementedError(f"_printStackFrame: unimplemented subroutine type {defn[0]}")
            else:
                args = "?"
            print(f"{' ' * (i * 2)}{name}({args}): fp={fp_str}, r7={r7_str}")

    def _saveRegisters(self):
        self.saved_registers = [self.sim.get_reg(i) for i in range(8)]

    def _assertShortEqual(self, expected: int, actual: int, msg_fmt: str | None = None, *, signed: bool = True, show_hex: bool = True):
        expected_i, expected_u = _to_i16(expected), _to_u16(expected)
        actual_i, actual_u = _to_i16(actual), _to_u16(actual)

        msg = (msg_fmt or "Expected {expected}, but got {actual}").format(
            expected=f"{expected_i if signed else expected_u}" + f" (x{expected_u:04X})" if show_hex else "",
            actual=f"{actual_i if signed else actual_u}" + f" (x{actual_u:04X})" if show_hex else ""
        )

        self.assertEqual(expected_u, actual_u, msg)
    
    def fillMachine(self, fill: core.MemoryFillType, value: int | None = None):
        if not isinstance(fill, core.MemoryFillType):
            raise InternalError(f"fillMachine argument 'fill' was expected to be {type(core.MemoryFillType)}, but was {type(fill)}")
        if isinstance(value, int) and not (0 <= value < 2 ** 64):
            raise InternalError(f"fillMachine argument 'value' must be an unsigned 64-bit integer")

        self.init_fill = fill
        self.init_seed = self.sim.init(fill, value)

    def loadFile(self, fp: str):
        self.source_code = Path(fp)
        self.sim.load_file(self.source_code)
    
    def loadCode(self, src: str):
        self.source_code = src
        self.sim.load_code(src)

    def _verify_ready_to_exec(self):
        if self.source_code is None:
            raise InternalError("cannot execute, no code was loaded")

    def runCode(self, max_instrs_run=INSTRUCTION_RUN_LIMIT):
        self._verify_ready_to_exec()
        self._saveRegisters()
        self.sim.run(max_instrs_run)

    def writeMemValue(self, label: str, value: int):
        addr = self._lookup(label)
        self.sim.write_mem(addr, _to_u16(value))
    
    def writeArray(self, label: str, lst: list[int]):
        addr = self._lookup(label)
        self._writeContiguous(addr, lst)

    def writeString(self, label: str, string: str):
        addr = self._lookup(label)
        string_bytes = _verify_ascii_string(string, arg_desc=f"string value parameter ({string=!r})")

        self._writeContiguous(addr, string_bytes)
        self.sim.write_mem(addr + len(string_bytes), 0)

    def setReg(self, reg_no: int, value: int):
        _verify_reg_no(reg_no)
        self.sim.set_reg(reg_no, _to_u16(value))

    def setInput(self, inp: str):
        _verify_ascii_string(inp, arg_desc=f"input parameter ({inp=!r})")
        self.sim.input = inp

    def assertReg(self, reg_no: int, expected: int):
        _verify_reg_no(reg_no)
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
        expected_bytes = _verify_ascii_string(expected_str, arg_desc=f"expected string parameter ({expected_str=!r})")
        
        expected = [*expected_bytes, 0]
        actual = list(self._readContiguous(addr, len(expected)))
        
        # Verify all (except last) elements are ASCII-compatible and not a null-terminator
        for i, ch in enumerate(actual[:-1]):
            if ch == 0:
                actual_str = bytes(actual[:i]).decode("ascii") # ok because we checked beforehand
                self.fail(
                    self._simpleAssertMsg(f"String at {label.upper()} shorter than expected",
                        f"{expected_str} {expected}",
                        f"{actual_str.ljust(len(expected_str))} {actual}")
                )
            elif not (0 <= ch <= 127):
                fail_array = f"[{', '.join(map(str, actual[:i + 1]))}, ...]"
                self.fail(f"Found invalid ASCII byte in string at label {label.upper()}: {fail_array}")

        # ok because we checked beforehand
        # the actual string doesn't include the last element, so we omit it in any following print statements
        actual_str = bytes(actual[:-1]).decode("ascii")

        # Verify last element is the null-terminator
        if actual[-1] != 0:
            self.fail(
                self._simpleAssertMsg(f"String at {label.upper()} longer than expected", 
                    f"{expected_str}    {expected}", 
                    f"{actual_str}... {actual}")
            )
        
        # Check for mismatches
        for e, a in zip(expected, actual):
            self.assertEqual(e, a,
                self._simpleAssertMsg(f"String at label {label.upper()} did not match expected", 
                    f"{expected_str} {expected}", 
                    f"{actual_str} {actual}"
                )
            )
    
    def assertConsoleOutput(self, expected: str):
        # There's technically nothing wrong with non-ASCII inputs for this method,
        # and it could accept non-ASCII text if it wanted

        # But just for consistency and too-lazy-to-verify-correctness,
        # we'll just require it's ASCII
        _verify_ascii_string(expected, arg_desc=f"expected string parameter ({expected=!r})")
        actual = self.sim.output
        self.assertEqual(expected, actual,
                         self._simpleAssertMsg("Console output did not match expected", expected, actual))

    def assertPC(self, expected: int):
        self._assertShortEqual(expected, self.sim.pc,
                                  f"Expected PC to be {{expected}}, but it was {{actual}}",
                                  signed = False)
    
    def assertCondCode(self, expected: typing.Literal["n", "z", "p"]):
        if expected not in ('n', 'z', 'p'): raise InternalError(f"expected parameter should be 'n', 'z', or 'p' ({expected=!r})")
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
    
    def defineSubroutine(self, loc: int | str, params: list[str] | dict[int, str], ret: int | None = None):
        if isinstance(params, list):
            defn = (core.SubroutineType.CallingConvention, params)
        else:
            param_list = [(v, k) for (k, v) in params.items()]
            defn = (core.SubroutineType.PassByRegister, param_list, ret)
        
        self.sim.set_subroutine_def(loc, defn)
    
    def _getReturnValue(self, callee: int) -> int | None:
        defn = self.sim.get_subroutine_def(callee)

        if defn is not None:
            if defn[0] == core.SubroutineType.CallingConvention:
                ret = self.sim.get_mem(self.sim.r6)
            elif defn[0] == core.SubroutineType.PassByRegister:
                _, _, ret_reg = defn
                ret = self.sim.get_reg(ret_reg) if ret_reg is not None else None
            else:
                raise NotImplementedError(f"_getReturnValue: unimplemented subroutine type {defn[0]}")
            
            return ret

    def callSubroutine(self, label: str, args: list[int]) -> list[CallNode]:
        self._verify_ready_to_exec()

        R5, R6, R7 = 0x5555, 0x6666, 0x7777
        
        addr = self._lookup(label)
        defn = self.sim.get_subroutine_def(addr)
        if defn is None:
            raise InternalError(
                f"No definition provided for subroutine {label.upper()!r}."
                "Provide one with self.defineSubroutine."
            )

        self.sim.r5 = R5
        self.sim.r6 = R6
        # Handle all arguments
        if defn[0] == core.SubroutineType.CallingConvention:
            params = defn[1]
            if len(params) != len(args):
                raise InternalError(
                    f"Number of arguments provided ({len(args)}) does not match "
                    f"the number of parameters subroutine {label.upper()!r} accepts ({len(params)})"
                )
            # Write arguments to stack
            self.sim.r6 -= len(args)
            self._writeContiguous(self.sim.r6, args)
        elif defn[0] == core.SubroutineType.PassByRegister:
            params = defn[1]
            if len(params) != len(args):
                raise InternalError(
                    f"Number of arguments provided ({len(args)}) does not match "
                    f"the number of parameters subroutine {label.upper()!r} accepts ({len(params)})"
                )
            # Write arguments to each register
            for (_, reg_no), arg in zip(params, args):
                self.sim.set_reg(reg_no, _to_u16(arg))
        else:
            raise NotImplementedError(f"callSubroutine: unimplemented subroutine type {defn[0]}")
        
        self.sim.pc = R7
        self._saveRegisters()
        self.sim.call_subroutine(addr)
        
        path: list[CallNode] = [CallNode(frame_no=self.sim.frame_number, callee=addr, args=list(args))]
        curr_path: list[CallNode] = [*path]

        while self.sim.frame_number >= path[0].frame_no and not self.sim.hit_halt():
            last_frame_no = self.sim.frame_number
            self.sim._run_until_frame_change()

            if self.sim.frame_number > last_frame_no:
                last_frame = self.sim.last_frame
                if last_frame is None: raise InternalError("cannot compute CallNode without debug_frames")
                
                node = CallNode(
                    frame_no=self.sim.frame_number, 
                    callee=last_frame.callee_addr, 
                    args=[d for d, _ in last_frame.arguments]
                )
                path.append(node)
                curr_path.append(node)
            
            if self.sim.frame_number < last_frame_no:
                node = curr_path.pop()
                node.ret = self._getReturnValue(node.callee)
        path[0].ret = self._getReturnValue(path[0].callee)

        if self.sim.hit_halt():
            self.fail(f"Program halted before completing execution of subroutine {label!r}")

        # TODO: better interface than list[CallNode]
        return path
    
    def assertCallsCorrect(self): pass
    def assertRegsPreserved(self, regs: list[int] | None = None):
        if regs is None:
            regs = [0, 1, 2, 3, 4, 5, 7]
        elif not all(0 <= r < 8 for r in regs):
                raise InternalError("regs argument has to consist of register numbers (which are between 0 and 7 inclusive)")
    
        if self.saved_registers is None:
            raise InternalError("cannot call assertRegsPreserved before an execution method (e.g., runCode or callSubroutine)")
        
        for r in regs:
            self._assertShortEqual(
                self.saved_registers[r],
                self.sim.get_reg(r),
                f"Expected registers to be preserved: register {r} was expected to be {{expected}}, but was {{actual}}"
            )
    def assertStackCorrect(self): pass
        
    def assertHalted(self):
        if not self.sim.hit_halt():
            self.fail("Program did not halt correctly")
    
    def assertReturned(self):
        self.assertPC(self.sim.r7)
