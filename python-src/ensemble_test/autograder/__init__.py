"""
Test case utility.
"""
from collections.abc import Iterable
import dataclasses
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

INSTRUCTION_RUN_LIMIT = 0xA_2110

def _to_u16(val: int) -> int:
    return val & 0xFFFF
def _to_i16(val: int) -> int:
    val = _to_u16(val)
    return val - (val >> 15) * 65536

class InternalArgError(Exception):
    """
    An error which occurs because of an illegal or malformed argument in a test case.

    It should not be possible to create this error due to some user input into the test case.
    """

    def __str__(self):
        return f"{super().__str__()}\n[This error should not occur. If you see this on Gradescope, contact a TA.]"

def _verify_ascii_string(string: str, *, arg_desc: str | None = None) -> bytes:
    try:
        bstring = string.encode("ascii")
    except UnicodeEncodeError:
        arg_desc = arg_desc or f"string parameter ({string=!r})"
        raise InternalArgError(f"{arg_desc} should be an ASCII string")

    if any(b == 0 for b in bstring):
        raise InternalArgError(f"{arg_desc} contains a null terminator")

    return bstring

def _verify_reg_no(reg_no: int):
    if not (0 <= reg_no <= 7):
        raise InternalArgError(f"cannot access non-existent register {reg_no}")

def _simple_assert_msg(msg, expected, actual):
    return (
        f"{msg if msg is not None else ""}\n"
        f"expected: {expected}\n"
        f"actual:   {actual}"
    )

def _nonnull_or_default(value, default):
    return value if value is not None else default

@dataclasses.dataclass
class CallNode:
    frame_no: int
    callee: int
    args: list[int]
    ret: int | None = None
CallTraceList = list[CallNode]

class _ExecRunCode(typing.NamedTuple):
    max_instrs_run: int
class _ExecCallSubroutine(typing.NamedTuple):
    label: str
    args: list[int]
    R6: int
    PC: int
    max_instrs_run: int
_ExecProperties = _ExecRunCode | _ExecCallSubroutine

MemLocation: typing.TypeAlias = "str | int | _LocatedInt"
class _IOriginRegister(typing.NamedTuple):
    # Item's origin (address) is Rx + offset
    reg_no: int
    offset: int
class _IOriginIndirect(typing.NamedTuple):
    # Item's origin (address) is mem[inner] + offset
    inner: MemLocation
    offset: int
_IOrigin = _IOriginRegister | _IOriginIndirect

class _LocatedInt(int):
    """
    An integer that keeps track (or atleast tries to keep track) of its origin.

    This class is intended to interact transparently with `int`s,
    and any operations that can be done with `int` is preserved.
    
    In other words, users of the test should not be able to distinguish this class
    from `int`.

    The origin is annotated with one of the following: 
    - register number + offset
    - memory label + offset
    - memory address + offset

    This class is useful for preserving origin information for errors and state validation
    in cases of indirect memory accesses while keeping the test case easy to read.

    Here is an example of what this class allows:
    ```py
    visited_addr: int = self.readMemValue("LABEL") # x9F9F
    self.assertMemValue(visited_addr + 2, x9494) # AssertionError: mem[mem[LABEL] + 2] is incorrect
    ```

    The origin is only preserved if `object`, `(object + integer offset)`, or `(object - integer offset)` 
    are input into a location parameter of a write* or assert* function. Any other operations
    (e.g., multiplication) will erase the origin.
    """
    _origin: _IOrigin | None
    def __new__(cls, value: int, *, origin: _IOrigin | None): 
        o = super().__new__(cls, value)
        o._origin = origin
        return o
    
    def _new_origin(self, offset: int) -> _IOrigin | None:
        if self._origin is None: return None
        (*rest, current_off) = self._origin
        return self._origin.__class__(*rest, current_off + offset) # type: ignore
    
    def __add__(self, other: int) -> typing.Self:
        return self.__class__(super().__add__(other), origin=self._new_origin(other))
    def __sub__(self, other: int) -> typing.Self:
        return self.__class__(super().__sub__(other), origin=self._new_origin(-other))

def _get_loc_name(loc: MemLocation) -> str:
    """
    Computes a descriptive name for a label or an address.

    Parameters
    ----------
    loc : str | int
        Location (either a label or an unsigned short address)

    Returns
    -------
    str
        The descriptive name.
        - For labels, this is simply the name of the label.
        - For addresses, this is the hex form of the address (e.g., `38807` -> `x9797`)
    """
    if isinstance(loc, _LocatedInt) and loc._origin is not None:
        origin = loc._origin
        if isinstance(origin, _IOriginRegister):
            name = f"R{origin.reg_no}"
        elif isinstance(origin, _IOriginIndirect):
            name = f"mem[{_get_loc_name(origin.inner)}]"
        else:
            raise ValueError(f"_LocatedInt origin should not have origin type {type(origin)}")
    
        if origin.offset < 0:
            name += f" - {abs(origin.offset)}"
        elif origin.offset > 0:
            name += f" + {origin.offset}"
            
        return name
    elif isinstance(loc, int):
        return f"x{_to_u16(loc):04X}"
    elif isinstance(loc, str):
        return loc.upper()
    else:
        raise ValueError(f"cannot find name of location of type {type(loc)}")

class LC3UnitTestCase(unittest.TestCase):
    def setUp(self):
        self.sim = core.Simulator()
        # State of all 8 registers before execution.
        # If an execution has not yet occurred, this is None.
        self.saved_registers: list[int] | None = None

        ##### Preconditions

        # Fill type used to initialize machine
        self.init_fill = core.MemoryFillType.Random
        
        # Fill seed used to initialize the machine.
        # This can be explicitly set by :method:`LC3UnitTestCase.fillMachine`.
        self.init_seed = self.sim.init(self.init_fill)
        
        # Where the source comes from.
        # - If this is a `Path`, it comes from some local file.
        # - If this is a `str`, it comes from inline code.
        # - If this is `None`, it has not yet been declared.
        self.source_code: Path | str | None = None

        ##### Execution

        # The execution type.
        # If this is None, then no execution has occurred.
        self.exec_props: _ExecProperties | None = None

        # If execution was a subroutine call,
        # this holds the return trace from that call.
        self.call_trace_list: CallTraceList | None = None
    
    ##### HELPERS #####

    def _readContiguous(self, start_addr: int, length: int | None = None):
        """
        Returns an iterator which returns a contiguous range of elements in memory.

        The range starts at `start_addr` and reads up to `length` elements (if length is `None`, it reads forever).
        """
        ctr = itertools.count() if length is None else range(length)
        return (self.sim.read_mem(_to_u16(start_addr + i)) for i in ctr)
    
    def _writeContiguous(self, start_addr: int, words: Iterable[int]):
        """
        Writes the given words into a contiguous range of elements in memory.

        The range starts at `start_addr` and writes until the iterable is depleted.
        """
        for i, word in enumerate(words):
            self.sim.write_mem(start_addr + i, _to_u16(word))

    def _lookup(self, label: str) -> int:
        """
        Converts a label to its corresponding address, raising an error if it does not exist.
        """
        addr = self.sim.lookup(label)
        if addr is None:
            # not InternalArgError because it's possible for this error to occur with user input
            # e.g., if user deletes the label
            raise ValueError(f"Label {label.upper()} is missing in the assembly code")

        return addr
    
    def _resolveAddr(self, loc: MemLocation) -> int:
        """
        Computes the address this memory location is equivalent to.
        
        Note that this function wipes any origin information 
        and therefore the result should not be input to any MemLocation parameters.
        """
        if isinstance(loc, str):
            return self._lookup(loc)
        elif isinstance(loc, int):
            return _to_u16(loc)
        else:
            raise ValueError(f"Cannot resolve location of type {type(loc)} into an address")

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

    def _load_from_src(self):
        """Loads from `self.source_code`, if present"""
        if isinstance(self.source_code, Path):
            self.sim.load_file(self.source_code)
        elif isinstance(self.source_code, str):
            self.sim.load_code(self.source_code)
        else:
            self.source_code = None

        # Reset these if successfully replaced the simulator's state
        self.saved_registers = None
        self.exec_props = None

    def _saveRegisters(self):
        self.saved_registers = [self.sim.get_reg(i) for i in range(8)]

    def _verify_ready_to_exec(self):
        """Verifies the simulator is in a state to execute code."""
        if self.source_code is None:
            raise InternalArgError("cannot execute, no code was loaded")
    
    def _getReturnValue(self, callee: int) -> int | None:
        """
        Pulls the return value following a subroutine call.
        """
        defn = self.sim.get_subroutine_def(callee)

        if defn is not None:
            if defn[0] == core.SubroutineType.CallingConvention:
                ret = self.sim.get_mem(self.sim.r6)
            elif defn[0] == core.SubroutineType.PassByRegister:
                _, _, ret_reg_no = defn
                ret = self.sim.get_reg(ret_reg_no) if ret_reg_no is not None else None
            else:
                raise NotImplementedError(f"_getReturnValue: unimplemented subroutine type {defn[0]}")
            
            return ret
    
    def _assertShortEqual(
            self, 
            expected: int, actual: int, 
            msg: str | None = None, *, 
            signed: bool = True, 
            show_hex: bool = True
        ):
        """
        Internal wrapper to check for short equality.

        Parameters
        ----------
        expected : int
            The expected value
        actual : int
            The user's actual resultant value
        msg : str, optional
            A custom message to print if the assertion fails
        signed : bool, optional
            Whether the displayed value is signed or unsigned, by default True
        show_hex : bool, optional
            Whether the displayed value should have its hex format printed, by default True
        """
        expected_i, expected_u = _to_i16(expected), _to_u16(expected)
        actual_i, actual_u = _to_i16(actual), _to_u16(actual)

        expected_n = expected_i if signed else expected_u
        actual_n = actual_i if signed else actual_u

        # create expected_str and actual_str, with format:
        #  expected:      0 (x0000)
        #  actual:   -29824 (x8B80)
        expected_str, actual_str = str(expected_n), str(actual_n)
        pad = max(len(expected_str), len(actual_str))
        expected_str, actual_str = expected_str.rjust(pad), actual_str.rjust(pad)
        if show_hex:
            expected_str += f" (x{expected_u:04X})"
            actual_str += f" (x{actual_u:04X})"
        
        # create msg and assert:
        msg = _simple_assert_msg(msg or "Shorts not equal", expected_str, actual_str)
        self.assertEqual(expected_n, actual_n, msg)
    
    ##### PRECONDITIONS #####

    def fillMachine(self, fill: core.MemoryFillType, value: int | None = None):
        """
        Resets the machine and configures how its memory and registers are filled.

        Parameters
        ----------
        fill : core.MemoryFillType
            The fill type
        value : int | None, optional
            The seed or value to fill with

        Raises
        ------
        InternalArgError
            If argument `fill` is not of type `core.MemoryFillType` or if value is not 
        InternalArgError
            If argument `value` is not an unsigned 64-bit integer or None
        """
        if not isinstance(fill, core.MemoryFillType):
            raise InternalArgError(f"fillMachine argument 'fill' was expected to be {type(core.MemoryFillType)}, but was {type(fill)}")
        if value is not None:
            if not (isinstance(value, int) and (0 <= value < 2 ** 64)):
                raise InternalArgError(f"fillMachine argument 'value' must be an unsigned 64-bit integer")

        self.init_fill = fill
        self.init_seed = self.sim.init(fill, value)
        self._load_from_src()

    def loadFile(self, fp: str):
        """
        Loads code into the simulator from a given file.

        Parameters
        ----------
        fp : str
            the file path to load from
        """
        self.source_code = Path(fp)
        self._load_from_src()
    
    def loadCode(self, src: str):
        """
        Loads code into the simulator from a given string.

        Parameters
        ----------
        src : str
            the string holding the source code to load from
        """
        self.source_code = src
        self._load_from_src()

    def readMemValue(self, loc: MemLocation) -> int:
        """
        Reads the memory value from a given location.

        Parameters
        ----------
        loc : MemLocation
            Label or address (as a unsigned short) of the memory location to read from.

        Returns
        -------
        int
            Value at the memory location.
        """
        addr = self._resolveAddr(loc)
        return _LocatedInt(self.sim.read_mem(addr), origin=_IOriginIndirect(loc, 0))
    
    def getReg(self, reg_no: int) -> int:
        """
        Reads the value from a given register.

        Parameters
        ----------
        reg_no : int
            Register to read.

        Returns
        -------
        int
            Value at the given register.
        """
        _verify_reg_no(reg_no)
        return _LocatedInt(self.sim.get_reg(reg_no), origin=_IOriginRegister(reg_no, 0))
    
    def writeMemValue(self, loc: MemLocation, value: int):
        """
        Writes a memory value into the provided label location.

        Parameters
        ----------
        loc : str | int
            Label or address (as a unsigned short) of the memory location to write to.
        value : int (unsigned short)
            Value to write.
        """
        addr = self._resolveAddr(loc)
        self.sim.write_mem(addr, _to_u16(value))
    
    def writeArray(self, loc: MemLocation, lst: list[int]):
        """
        Writes a contiguous sequence of memory values (an array) starting at the provided label location.

        Parameters
        ----------
        loc : str | int
            Label or address (as a unsigned short) of the location to write an array to.
        lst : list[int] (list[unsigned short])
            Array to write.
        """
        addr = self._resolveAddr(loc)
        self._writeContiguous(addr, lst)

    def writeString(self, loc: MemLocation, string: str):
        """
        Writes a null-terminated string into memory starting at the provided label location.

        Parameters
        ----------
        loc : str | int
            Label or address (as a unsigned short) of the location to write a string to.
        string : str
            String to write (must be ASCII).
            A null terminator will be added to the end of this string.
        """
        addr = self._resolveAddr(loc)
        string_bytes = _verify_ascii_string(string, arg_desc=f"string value parameter ({string=!r})")

        self._writeContiguous(addr, string_bytes)
        self.sim.write_mem(addr + len(string_bytes), 0)

    def setReg(self, reg_no: int, value: int):
        """
        Sets the value of a register.

        Parameters
        ----------
        reg_no : int
            Register to set.
        value : int (unsigned short)
            Value to set register to.
        """
        _verify_reg_no(reg_no)
        self.sim.set_reg(reg_no, _to_u16(value))

    def setInput(self, inp: str):
        """
        Sets the current input string.

        Parameters
        ----------
        inp : str
            String to set the input to (must be ASCII).
        """
        _verify_ascii_string(inp, arg_desc=f"input parameter ({inp=!r})")
        self.sim.input = inp


    def defineSubroutine(self, loc: MemLocation, params: list[str] | dict[int, str], ret: int | None = None):
        """
        Defines a subroutine signature to be called in `self.callSubroutine`.

        Parameters
        ----------
        loc : str | int
            Location of subroutine (either a label or unsigned short address)
        params : list[str] | dict[int, str]
            The parameters of the subroutine.

            There are two forms of subroutine, which are determined by the type of this parameter.
            - Standard LC-3 calling convention: The parameters are given by a list of parameter names.
            - Pass-by-register calling convention: The parameters are given by a dict, which holds a mapping from register number to parameter names.
        ret : int | None, optional
            This is not used in standard LC-3 calling convention (as a return value location is present and always known).
            In pass-by-register calling convention, this is used to designate which register holds the return value.

        Examples
        --------
        ```py
        # define a 3-ary function with standard LC-3 calling convention
        self.defineSubroutine("SR_STD", params=["a", "b", "c"])

        # define a 2-ary function with pass-by-register calling convention
        # taking parameters at (R0, R1) and returning in R0.
        self.defineSubroutine("SR_PBR", params={0: "a", 1: "b"}, ret=0)
        ```
        """
        if isinstance(params, list):
            defn = (core.SubroutineType.CallingConvention, params)
        elif isinstance(params, dict):
            param_list = [(v, k) for (k, v) in params.items()]
            defn = (core.SubroutineType.PassByRegister, param_list, ret)
        else:
            raise InternalArgError(f"Cannot define subroutine with parameters {params}")
        
        self.sim.set_subroutine_def(loc, defn)

    ##### EXECUTION #####

    def runCode(self, max_instrs_run=INSTRUCTION_RUN_LIMIT):
        """
        Runs the code.

        Parameters
        ----------
        max_instrs_run : int, optional
            The maximum number of instructions to run before forcibly stopping, 
            by default `INSTRUCTION_RUN_LIMIT`
        """
        self._verify_ready_to_exec()
        self._saveRegisters()
        self.exec_props = _ExecRunCode(max_instrs_run)
        self.sim.run(max_instrs_run)


    def callSubroutine(self, label: str, args: list[int], R6 = 0x6666, PC = 0x7777, max_instrs_run=INSTRUCTION_RUN_LIMIT) -> CallTraceList:
        """
        Calls a subroutine with the provided arguments.

        This loads the arguments into the LC-3 machine, executes the subroutine,
        and returns the resulting list of subroutine calls that occur as a result of this call.

        Parameters
        ----------
        label : str
            The label where the subroutines are located.
        args : list[int] (list[unsigned short])
            The arguments to call the subroutine with.
        R6: int (unsigned short), optional
            The initial value of R6 prior to this subroutine call.
        PC: int (unsigned short), optional
            The initial value of the PC prior to this subroutine call.
        max_instrs_run : int, optional
            The maximum number of instructions to run before forcibly stopping, 
            by default `INSTRUCTION_RUN_LIMIT`

        Returns
        -------
        list[CallNode]
            The list of calls performed as a result of this subroutine call.

        Raises
        ------
        InternalArgError
            If no subroutine definition is provided (use self.defineSubroutine to define one),
            or if the number of arguments provided differ from the number of arguments in the subroutine's definition,
            or if the simulator does not have the debug_frames flag enabled
        """
        self._verify_ready_to_exec()
        
        addr = self._lookup(label)
        defn = self.sim.get_subroutine_def(addr)
        if defn is None:
            raise InternalArgError(
                f"No definition provided for subroutine {label.upper()!r}."
                "Provide one with self.defineSubroutine."
            )

        self.sim.r6 = R6
        # Handle all arguments
        if defn[0] == core.SubroutineType.CallingConvention:
            params = defn[1]
            if len(params) != len(args):
                raise InternalArgError(
                    f"Number of arguments provided ({len(args)}) does not match "
                    f"the number of parameters subroutine {label.upper()!r} accepts ({len(params)})"
                )
            # Write arguments to stack
            self._writeContiguous(self.sim.r6 - len(args), args)
            self._saveRegisters()
            self.sim.r6 -= len(args)
        elif defn[0] == core.SubroutineType.PassByRegister:
            params = defn[1]
            if len(params) != len(args):
                raise InternalArgError(
                    f"Number of arguments provided ({len(args)}) does not match "
                    f"the number of parameters subroutine {label.upper()!r} accepts ({len(params)})"
                )
            # Write arguments to each register
            for (_param_names, reg_no), arg in zip(params, args):
                self.sim.set_reg(reg_no, _to_u16(arg))
            self._saveRegisters()
        else:
            raise NotImplementedError(f"callSubroutine: unimplemented subroutine type {defn[0]}")
        
        self.sim.pc = PC
        self.sim.write_mem(PC, self.sim.read_mem(PC)) # initialize this location so that it doesn't crash when calling in strict mode

        self.exec_props = _ExecCallSubroutine(label, args, R6, PC, max_instrs_run)
        self.sim.call_subroutine(addr)
        
        path: list[CallNode] = [CallNode(frame_no=self.sim.frame_number, callee=addr, args=list(args))]
        curr_path: list[CallNode] = [*path]

        start = self.sim.instructions_run
        while self.sim.frame_number >= path[0].frame_no and not self.sim.hit_halt() and self.sim.instructions_run - start < max_instrs_run:
            last_frame_no = self.sim.frame_number
            self.sim._run_until_frame_change(start + max_instrs_run)

            # we stepped into a subroutine
            if self.sim.frame_number > last_frame_no:
                last_frame = self.sim.last_frame
                if last_frame is None: raise InternalArgError("cannot compute CallNode without debug_frames")
                
                node = CallNode(
                    frame_no=self.sim.frame_number, 
                    callee=last_frame.callee_addr, 
                    args=[d for d, _ in last_frame.arguments]
                )
                path.append(node)
                curr_path.append(node)
            
            # we stepped out of a subroutine
            if self.sim.frame_number < last_frame_no:
                node = curr_path.pop()
                node.ret = self._getReturnValue(node.callee)
        
        # if subroutine successfully returned, compute return value
        if self.sim.frame_number < path[0].frame_no:
            path[0].ret = self._getReturnValue(path[0].callee)

        if self.sim.hit_halt():
            self.fail(f"Program halted before completing execution of subroutine {label!r}")

        if defn[0] == core.SubroutineType.CallingConvention:
            # Pop return value and arguments
            # Offset is used for self.assertStackCorrect
            self.sim.r6 += len(args) + 1

        # TODO: better interface than list[CallNode]
        self.call_trace_list = CallTraceList(path)
        return self.call_trace_list
    
    ##### ASSERTIONS #####

    def assertReg(self, reg_no: int, expected: int, msg_fmt: str | None = None):
        """
        Asserts the value at the provided register number matches the expected value.

        Parameters
        ----------
        reg_no : int
            Register to check.
        expected : int (unsigned short)
            The expected value.
        msg_fmt: str, optional
            A custom message to print if the assertion fails.
            {0} can be used in the message format to display the register number.
        """
        _verify_reg_no(reg_no)
        actual = self.sim.get_reg(reg_no)

        msg = _nonnull_or_default(msg_fmt, "Incorrect value for register {}").format(reg_no)
        self._assertShortEqual(expected, actual, msg)
    
    def assertMemValue(self, loc: MemLocation, expected: int, msg_fmt: str | None = None):
        """
        Asserts the value at the provided label matches the expected value.

        Parameters
        ----------
        loc: MemLocation
            Label or address (as a unsigned short) of the memory location to check.
        expected : int (unsigned short)
            The expected value.
        msg_fmt: str, optional
            A custom message to print if the assertion fails.
            {0} can be used in the message format to display the label of the value.
        """
        addr = self._resolveAddr(loc)
        actual = self.sim.read_mem(addr)

        msg = _nonnull_or_default(msg_fmt, "Incorrect value for mem[{}]").format(_get_loc_name(loc))
        self._assertShortEqual(expected, actual, msg)

    def assertArray(self, loc: MemLocation, arr: list[int], msg_fmt: str | None = None):
        """
        Asserts the sequence of values (array) at the provided label matches the expected array of values.

        Parameters
        ----------
        loc: MemLocation
            Label or address (as a unsigned short) of the location of the array.
        arr : list[int] (list[unsigned short])
            The expected sequence of values.
        msg_fmt: str, optional
            A custom message to print if the assertion fails.
            {0} can be used in the message format to display the label of the array.
        """
        addr = self._resolveAddr(loc)
        
        expected = [_to_u16(e) for e in arr]
        actual = list(self._readContiguous(addr, len(arr)))

        msg = _nonnull_or_default(msg_fmt, "Array at location {} did not match expected").format(_get_loc_name(loc))
        self.assertEqual(expected, actual, _simple_assert_msg(msg, expected, actual))

    def assertString(self, loc: MemLocation, expected_str: str):
        """
        Asserts the string at the provided label matches the expected string and correctly includes the null -terminator.

        Parameters
        ----------
        loc: MemLocation
            Label or address (as a unsigned short) of the location the string to check.
        expected_str : str
            The expected string (must be ASCII).
        """
        addr = self._resolveAddr(loc)
        loc_name = _get_loc_name(loc)
        expected_bytes = _verify_ascii_string(expected_str, arg_desc=f"expected string parameter ({expected_str=!r})")
        expected = [*expected_bytes, 0]
        actual = list(self._readContiguous(addr, len(expected)))
        
        # Verify all (except last) elements are ASCII-compatible and not a null-terminator
        for i, ch in enumerate(actual[:-1]):
            if ch == 0:
                actual_str = bytes(actual[:i]).decode("ascii") # ok because we checked beforehand
                self.fail(
                    _simple_assert_msg(f"String at {loc_name} shorter than expected",
                        f"{expected_str} {expected}",
                        f"{actual_str.ljust(len(expected_str))} {actual}")
                )
            elif not (0 <= ch <= 127):
                fail_array = f"[{', '.join(map(str, actual[:i + 1]))}, ...]"
                self.fail(f"Found invalid ASCII byte in string at location {loc_name}: {fail_array}")

        # ok because we checked beforehand
        # the actual string doesn't include the last element, so we omit it in any following print statements
        actual_str = bytes(actual[:-1]).decode("ascii")

        # Verify last element is the null-terminator
        if actual[-1] != 0:
            self.fail(
                _simple_assert_msg(f"String at {loc_name} longer than expected", 
                    f"{expected_str}    {expected}", 
                    f"{actual_str}... {actual}")
            )
        
        # Check for mismatches
        for e, a in zip(expected, actual):
            self.assertEqual(e, a,
                _simple_assert_msg(f"String at location {loc_name} did not match expected", 
                    f"{expected_str} {expected}", 
                    f"{actual_str} {actual}"
                )
            )
    
    def assertOutput(self, expected: str, msg: str | None = None):
        """
        Assert the current output string matches the expected string.

        Parameters
        ----------
        expected : str
            The expected string.
        msg: str, optional
            A custom message to print if the assertion fails.
        """
        # There's technically nothing wrong with non-ASCII inputs for this method,
        # and it could accept non-ASCII text if it wanted

        # But just for consistency and too-lazy-to-verify-correctness,
        # we'll just require it's ASCII
        _verify_ascii_string(expected, arg_desc=f"expected string parameter ({expected=!r})")
        actual = self.sim.output
        self.assertEqual(expected, actual,
            _simple_assert_msg(
                _nonnull_or_default(msg, "Console output did not match expected"), 
                expected, 
                actual
            )
        )

    def assertPC(self, expected: int, msg: str | None = None):
        """
        Assert the PC matches the expected value.

        Parameters
        ----------
        expected : int (unsigned short)
            The expected address of the PC.
        msg: str, optional
            A custom message to print if the assertion fails.
        """
        self._assertShortEqual(expected, self.sim.pc,
            _nonnull_or_default(msg, f"Incorrect value for PC"),
            signed = False
        )
    
    def assertCondCode(self, expected: typing.Literal["n", "z", "p"], msg_fmt: str | None = None):
        """
        Assert the condition code matches the expected condition code.

        Parameters
        ----------
        expected : typing.Literal["n", "z", "p"]
            The expected condition code.
        msg_fmt: str, optional
            A custom message to print if the assertion fails.
            {0} and {1} can be used in the message format to display the expected and actual condition codes.
        """
        if expected not in ('n', 'z', 'p'): 
            raise InternalArgError(f"expected parameter should be 'n', 'z', or 'p' ({expected=!r})")
        n, z, p = self.sim.n, self.sim.z, self.sim.p

        # These should not occur; something has gone severely wrong.
        if n + z + p < 1:
            raise InternalArgError("Simulation error: None of the condition codes are enabled")
        if n + z + p > 1:
            raise InternalArgError(f"Simulation error: More than 1 condition code is enabled ({n=}, {z=}, {p=})")

        if n:
            actual = "n"
        elif z:
            actual = "z"
        else:
            actual = "p"

        msg = _nonnull_or_default(msg_fmt, "Incorrect condition code (expected {}, got {})").format(repr(expected), repr(actual))
        self.assertEqual(expected, actual, msg)
    
    def assertRegsPreserved(self, regs: list[int] | None = None, msg_fmt: str | None = None):
        """
        Asserts the values of the given registers are unchanged after an execution.

        Parameters
        ----------
        regs : list[int], optional
            List of registers to verify were preserved.
            If not provided, this defaults to all registers except R6.
        msg_fmt: str, optional
            A custom message to print if the assertion fails.
            {0} can be used in the message format to display the first mismatching register.

        Raises
        ------
        InternalArgError
            If the register numbers provided exceed normal register numbers (e.g., less than 0 or greater than 7),
            or if this function is called before an execution call.
        """
        if regs is None:
            regs = [0, 1, 2, 3, 4, 5]
        elif not all(0 <= r < 8 for r in regs):
                raise InternalArgError("regs argument has to consist of register numbers (which are between 0 and 7 inclusive)")
    
        if self.exec_props is None or self.saved_registers is None:
            raise InternalArgError("cannot call assertRegsPreserved before an execution method (e.g., runCode or callSubroutine)")
        
        for r in regs:
            msg =  _nonnull_or_default(msg_fmt, "Registers changed after execution: incorrect value for register {}").format(r)
            self._assertShortEqual(self.saved_registers[r], self.sim.get_reg(r), msg)
    
    def assertStackCorrect(self):
        """
        Asserts the stack is managed correctly after a subroutine call.

        Raises
        ------
        InternalArgError
            If a call to this function wasn't preceded by a self.callSubroutine execution.
        """
        if not isinstance(self.exec_props, _ExecCallSubroutine) or self.saved_registers is None:
            raise InternalArgError("self.assertStackCorrect can only be called after self.callSubroutine")
        
        # This should check for overflow, 
        # but that is such a degenerate case that it's probably fine to ignore.
        orig_sp  = _to_u16(self.saved_registers[6])
        final_sp = _to_u16(self.sim.r6)

        if final_sp < orig_sp:
            self.fail(f"Stack was not managed properly for subroutine {self.exec_props.label!r}: there were more items remaining in the stack than expected")
        if final_sp > orig_sp:
            self.fail(f"Stack was not managed properly for subroutine {self.exec_props.label!r}: there were fewer items remaining in the stack than expected")
        
    def assertHalted(self, msg: str | None = None):
        """
        Asserts that the program halted correctly.
        
        Parameters
        ----------
        msg: str, optional
            A custom message to print if the assertion fails.

        Raises
        ------
        InternalArgError
            If a call to this function wasn't preceded by a self.runCode execution.
        """
        if not isinstance(self.exec_props, _ExecRunCode):
            raise InternalArgError(
                "self.assertHalted can only be called after self.runCode.\n"
                "If you meant to check if the subroutine returned, use self.assertReturned."
            )
        
        if not self.sim.hit_halt():
            self.fail(_nonnull_or_default(msg, "Program did not halt correctly"))
    
    def assertReturned(self, msg: str | None = None):
        """
        Asserts that the execution returned correctly.
        
        Parameters
        ----------
        msg: str, optional
            A custom message to print if the assertion fails.

        Raises
        ------
        InternalArgError
            If a call to this function wasn't preceded by a self.callSubroutine execution.
        """
        if not isinstance(self.exec_props, _ExecCallSubroutine):
            raise InternalArgError(
                "self.assertReturned can only be called after self.callSubroutine.\n"
                "If you meant to check if the subroutine halted, use self.assertHalted."
            )
        
        self.assertPC(self.sim.r7, _nonnull_or_default(msg, "Subroutine did not return properly"))
    
    def assertReturnValue(self, expected: int, msg: str | None = None):
        """
        Asserts the return value of a subroutine call is correct.

        Parameters
        ----------
        expected : int
            The expected return value of a subroutine call.
        msg : str | None, optional
            A custom message to print if the assertion fails.

        Raises
        ------
        InternalArgError
            If a call to this function wasn't preceded by a self.callSubroutine execution.
        """

        if not isinstance(self.exec_props, _ExecCallSubroutine) or self.call_trace_list is None:
            raise InternalArgError(
                "self.assertReturnValue can only be called after self.callSubroutine."
            )
        
        actual = self.call_trace_list[0].ret

        if actual is None:
            # I don't think this is reachable except by invalid arguments
            # Subroutines without return values shouldn't call this function ever(?)
            self.fail(_nonnull_or_default(msg, f"Subroutine unexpectedly did not return a value"))
        
        self._assertShortEqual(expected, actual, _nonnull_or_default(msg, "Incorrect return value"))
    
    def assertSubroutineCalled(self, label: str, msg_fmt: str | None = None):
        """
        Asserts that a subroutine call correctly called another subroutine.
        
        For example, if a helper subroutine `"BAR"` is expected to be used in subroutine `"FOO"`,
        this could be done by producing:
        ```py
            self.callSubroutine("FOO", [ ... ])
            self.assertSubroutineCalled("BAR")
        ```

        Parameters
        ----------
        label : str
            Name/label of the subroutine that we expect to have been called.
        msg_fmt : str | None, optional
            A custom message to print if the assertion fails.
            {0}, {1} represent the name of the subroutine called and the expected subroutine to be called respectively.

        Raises
        ------
        InternalArgError
            If a call to this function wasn't preceded by a self.callSubroutine execution,
            or if the provided label does not exist.
        """
        
        if not isinstance(self.exec_props, _ExecCallSubroutine) or self.call_trace_list is None:
            raise InternalArgError(
                "self.assertSubroutineCalled can only be called after self.callSubroutine."
            )
        
        caller = self.exec_props.label
        callee_addr = self._lookup(label)

        msg = _nonnull_or_default(msg_fmt, "Subroutine {} did not call {}").format(repr(caller), repr(label))
        if not any(c.callee == callee_addr for c in self.call_trace_list[1:]):
            self.fail(msg)
