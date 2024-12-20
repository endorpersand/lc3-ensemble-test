# Ensemble Autograder API & Development Guide

## Introduction

This document intends to give a brief summary of all of the available methods provided by the `lc3-ensemble-test` autograder API and an overview of the typical flow of an LC3 test case/suite. Note that the target audience for this document are those who intend to **design** autograders for LC3, not those who are **using** them.

If you want to see some examples of the autograder in action, see:

- [`examples/0-template`](../../examples/0-template): Autograder templates
- [`examples/`](../../examples/): Autograder examples

If you want a full description of the methods, these are provided in the docs of each function.

## Workflow

The `lc3-ensemble-test` autograder API builds upon Python's [`unittest`](https://docs.python.org/3/library/unittest.html#module-unittest) library, which allows it to use any libraries compatible with `unittest`. In particular, we use [`pytest`](https://docs.pytest.org/en/6.2.x/contents.html) due to its clean formatting and extensibility.

### Setup

To construct a test suite, we first create a Python file starting with `test_` (e.g., `test_gcd.py`, `test_add.py`, `test_autograder.py`). We fill out this file with the following:

```py
# test_sample.py
from ensemble_test import core, autograder
from parameterized import parameterized

class SampleTestSuite(autograder.LC3UnitTestCase):
    def setUp(self):
        super().setUp()

        # any initialization steps applicable to all test cases
        self.loadFile("sample.asm")
```

In this class, we put in all of our individual test cases. Note that with `unittest`, any method starting with `test_` is considered a test case, and any other method is not. A typical test case method will look like the following:

```py
@parameterized.expand([
    (arg1, arg2),
    (arg3, arg4),
    ...
],
    name_func=autograder.parameterized_name,
    doc_func=autograder.parameterized_doc
)
def test_foo(self, param1, param2):
    """
    (foo) Description of Foo, using arguments {0} and {1}
    """
```

Here, `parameterized.expand` is a utility function from [`parameterized`](https://pypi.org/project/parameterized/) which allows us to apply a test case to multiple sets of arguments.

For example, the declaration above will create two separate test cases:

- `test_foo(arg1, arg2)`, and
- `test_foo(arg3, arg4)`.

The `name_func` parameter configures the names of the test cases created by `parameterized`. The `autograder.parameterized_name` utility function sets the name to include the parameters used (instead of the default behavior of enumerating the separate test cases). This means the two separate test cases will have the IDs:

- `test_foo_arg1_arg2`, and
- `test_foo_arg3_arg4`.

The `doc_func` parameter configures the descriptions of the test cases created by `parameterized`. The `autograder.parameterized_doc` utility function allows developers to include arguments in the doc by using `{0}` and `{1}` format string syntax.

### Testing Programs

To test programs, we use the `autograder.LC3UnitTestCase.runCode` method. This method also asserts that no errors occur (namely, access violations and strictness memory errors) during the execution of the program.

This method does **not** implicitly assert that the program halts. To do that, we should include a call to `autograder.LC3UnitTestCase.assertHalted`.

```py
def test_foo(self, param1, param2):
    # ...

    self.runCode()
    self.assertHalted()

    # any further asserts
    self.assertReg(0, 0)
    self.assertReg(1, 1)
    # etc.
```

### Testing Subroutines

Beyond testing programs, we can also test subroutines via the `autograder.LC3UnitTestCase.callSubroutine`. Subroutines support both standard LC3 calling convention and pass-by-register calling convention.

`autograder.LC3UnitTestCase.callSubroutine` accepts a label (describing the name of the subroutine) and a list of arguments to the subroutine.

In order to use a subroutine in `callSubroutine`, it has to be defined via `defineSubroutine` (this allows the autograder to know a given subroutine's calling convention, argument order, parameter names, and return location (if applicable)). This typically can be done in `setUp` after a call to `loadCode` or `loadFile`.

```py
def setUp(self):
    super().setUp()
    self.loadFile("sample.asm")
    self.defineSubroutine("FOO", ["a", "b"])
    self.defineSubroutine("BAR", ["a", "b"])
```

`callSubroutine` asserts that no errors occur during the subroutine's execution (e.g., access violation or strict memory errors), but does **not** assert a successful return. To do that, include a call to `autograder.LC3UnitTestCase.assertReturned`.

```py
def test_foo(self, param1, param2):
    # ...

    self.callSubroutine("FOO", [0, 1])
    self.assertReturned()

    # any further asserts
    self.assertReg(0, 0)
    self.assertReg(1, 1)
    # etc.
```

One can also check the value returned by a subroutine with `autograder.LC3UnitTestCase.assertReturnValue`.

## Initialization

These are methods which initialize the machine and prepare it for a state of being executed. For the vast majority of autograders, it is required to call at least one of `loadCode` or `loadFile`.

### autograder.LC3UnitTestCase.fillMachine(fill: core.MemoryFillType, value: int | None)

Resets the machine, wiping any previously loaded object file, and fills the memory and registers.

There are several options:

- `(core.MemoryFillType.Single, uint16)`: Fills all memory and registers with `value`
- `(core.MemoryFillType.Single, None)`: Fills all memory and registers with the exact same randomly generated value
- `(core.MemoryFillType.Random, uint64)`: Randomly fills all memory and registers, using `value` as a seed
- `(core.MemoryFillType.Random, None)`: Randomly fills all memory and registers without using a seed

### autograder.LC3UnitTestCase.loadFile(fp: str)

Loads an object file from an `.asm` file on disk.

This function takes a filepath and raises an error if assembling fails.

### autograder.LC3UnitTestCase.loadCode(src: str)

Loads an object file from a string containing LC-3 assembly.

This function raises an error if assembling fails.

### autograder.LC3UnitTestCase.defineSubroutine(loc: int | str, params: list[str] | dict[int, str], ret: int | None)

Defines a subroutine signature, which allows it to be called in `self.callSubroutine`. This currently accepts two different types of subroutines:

- **Standard LC-3 calling convention**: Defined by passing a list of parameter names to `params`, and `None` to `ret`
- **Pass-by-register calling convention**: Defined by passing a dict of registers to parameter names to `params` and a register number (or `None`) to `ret`

This has to be called after `loadCode` or `loadFile`.

Examples:

```py
# define a 3-ary function with standard LC-3 calling convention
self.defineSubroutine("SR_STD", params=["a", "b", "c"])

# define a 2-ary function with pass-by-register calling convention
# taking parameters at (R0, R1) and returning in R0.
self.defineSubroutine("SR_PBR", params={0: "a", 1: "b"}, ret=0)
```

## Reads

These are methods which allow you to read data from the simulator.

### autograder.LC3UnitTestCase.readMemValue(loc: int | str) -> int

Reads the memory value from a given memory location (either a label or an address).

### autograder.LC3UnitTestCase.getReg(reg_no: int) -> int

Reads the value from a register.

## Writes

These are methods which allow you to write data from the simulator.

### autograder.LC3UnitTestCase.writeMemValue(loc: int | str, value: int)

Writes a memory value to a given memory location (either a label or an address).

### autograder.LC3UnitTestCase.writeArray(loc: int | str, lst: list[int])

Writes an array (a contiguous sequence of memory values) starting at the given memory location (either a label or an address).

### autograder.LC3UnitTestCase.writeString(loc: int | str, string: str)

Writes a null-terminated string into memory starting at the provided memory location.

### autograder.LC3UnitTestCase.setReg(reg_no: int, value: int)

Writes a value to a register.

### autograder.LC3UnitTestCase.setInput(inp: str)

Sets the keyboard input (i.e., what was typed into the simulator) to a given string.

### autograder.LC3UnitTestCase.setPC(pc: str)

Sets the program counter to the given address.

## Executions

These are methods which actually execute code. The state of the simulator after an execution can be checked with assertions.

### autograder.LC3UnitTestCase.runCode()

Runs the code.

### autograder.LC3UnitTestCase.callSubroutine(label: str, args: list[int]) -> CallTraceList

Calls a subroutine at the provided `label` with the provided arguments `args`.

To load the arguments into the LC-3 machine, this function needs to know the definition of the subroutine (which can be defined by `self.defineSubroutine`).

## Assertions

These are methods which assert properties about the state of the simulator (typically after an execution).

### autograder.LC3UnitTestCase.assertReg(reg_no: int, expected: int)

Asserts the value in a register matches an expected value.

### autograder.LC3UnitTestCase.assertMemValue(loc: int | str, expected: int)

Asserts the value in a given memory location (label or address) matches an expected value.

### autograder.LC3UnitTestCase.assertArray(loc: int | str, arr: list[int])

Asserts that the sequence of values (array) at a given memory location (label or address) matches an expected array of values.

### autograder.LC3UnitTestCase.assertString(loc: int | str, expected_str: str)

Asserts that the string at the provided label matches an expected string and correctly includes the null-terminator.

### autograder.LC3UnitTestCase.assertInput(expected: str)

Asserts the keyboard input matches an expected string.

### autograder.LC3UnitTestCase.assertOutput(expected: str)

Asserts the display output matches an expected string.

### autograder.LC3UnitTestCase.assertPC(expected: int)

Asserts the PC matches an expected value.

### autograder.LC3UnitTestCase.assertCondCode(expected: Literal["n", "z", "p"])

Asserts the current condition code matches an expected condition code.

### autograder.LC3UnitTestCase.assertRegsPreserved(regs: list[int] | None)

Asserts the values of the registers are unchanged after an execution.

This method can only be called after an execution.

### autograder.LC3UnitTestCase.assertStackCorrect()

Asserts that the stack is managed correctly after a subroutine call.

This method can only be called after `self.callSubroutine`.

### autograder.LC3UnitTestCase.assertHalted()

Asserts that the program halted (and did not fall into an infinite loop).

This method can only be called after `self.runCode`.
The `self.callSubroutine` equivalent of this function is `assertReturned`.

### autograder.LC3UnitTestCase.assertReturned()

Asserts that the subroutine returned (and did not fall into an infinite loop).

This method can only be called after `self.callSubroutine`.
The `self.runCode` equivalent of this function is `assertHalted`.

### autograder.LC3UnitTestCase.assertReturnValue(expected: int)

Asserts that the return value of a subroutine is an expected value.

This method can only be called after `self.callSubroutine`.

### autograder.LC3UnitTestCase.assertSubroutineCalled(label: str, args: list[int] | None = ..., *, directly_called: bool = ...)

Asserts that a subroutine was correctly called during an execution.

For example, if a helper subroutine `"BAR"` is expected to be used in subroutine `"FOO"`,
this could be done by producing:

```py
    self.callSubroutine("FOO", [ ... ])
    self.assertSubroutineCalled("BAR")
```

If we want to assert a specific argument was called, we can do that as well:

```py
    # FOO(N) must call BAR(N).
    self.callSubroutine("FOO", [ N ])
    self.assertSubroutineCalled("BAR", [ N ])
```

Note that by default, `assertSubroutineCalled` requires that the execution calls the expected callee at the top-level.
If you wish to require that a given subroutine is called *at all* during an execution, set the argument `directly_called` to `False`.

```text
FOO:
    JSR BAR ;; BAR is directly called by FOO
    RET

FOO2:
    JSR BAR2 ;; BAR2 is directly called by FOO2
    RET
BAR2:
    JSR BAZ ;; BAZ is directly called by BAR2, indirectly called by FOO2
    RET
BAZ:
    RET
```

This method can only be called after an execution. Additionally, the `label` argument must point to a subroutine defined using `self.defineSubroutine`.

### autograder.LC3UnitTestCase.assertSubroutinesCalledInOrder(calls: list[str | tuple[str, list[int] | None]])

Asserts that a given list of subroutines were called in order during execution.

For example, given the pseudocode:

```text
FOO:
    JSR PRINT, 0
    JSR BAR
    JSR PRINT, 4
    RET
BAR:
    JSR PRINT, 1
    JSR BAZ
    JSR PRINT, 3
    RET
BAZ:
    JSR PRINT, 2
    RET
```

This call order can be asserted with:

```py
self.callSubroutine("FOO", [ ... ])
self.assertSubroutinesCalledInOrder([
    "FOO", 
    ("PRINT", [0]),
    "BAR",
    ("PRINT", [1]),
    "BAZ",
    ("PRINT", [2]),
    ("PRINT", [3]),
    ("PRINT", [4]),
])
```

Or if we only want to assert the `PRINT` calls:

```py
self.callSubroutine("FOO", [ ... ])
self.assertSubroutinesCalledInOrder([
    ("PRINT", [0]),
    ("PRINT", [1]),
    ("PRINT", [2]),
    ("PRINT", [3]),
    ("PRINT", [4]),
])
```

This method can only be called after an execution. Additionally, the subroutine arguments must point to a subroutine defined using `self.defineSubroutine`.

## Internal

While not recommended, the autograder can also access the simulator and call methods on the simulator.

Certain simulator methods are exposed as test case methods and should be called from `autograder.LC3UnitTestCase`:

- `core.Simulator.init` => `autograder.LC3UnitTestCase.fillMachine`
- `core.Simulator.load_file` => `autograder.LC3UnitTestCase.loadFile`
- `core.Simulator.load_code` => `autograder.LC3UnitTestCase.loadCode`
- `core.Simulator.run` => `autograder.LC3UnitTestCase.runCode`
- `core.Simulator.read_mem` => `autograder.LC3UnitTestCase.readMemValue`
- `core.Simulator.write_mem` => `autograder.LC3UnitTestCase.writeMemValue`
- `core.Simulator.get_reg` => `autograder.LC3UnitTestCase.getReg`
- `core.Simulator.set_reg` => `autograder.LC3UnitTestCase.setReg`
- `core.Simulator.call_subroutine` => `autograder.LC3UnitTestCase.callSubroutine`

### core.Simulator.step_in(), core.Simulator.step_out(), core.Simulator.step_over()

Stepping functions.

### core.Simulator.get_mem(addr: int) -> int

Gets the memory value at a given address without triggering I/O side effects.

### core.Simulator.set_mem(addr: int, val: int)

Sets the memory value at a given address without triggering I/O side effects.

### core.Simulator.{r0, r1, r2, r3, r4, r5, r6, r7} (properties)

Properties to read from and write to each register.

### core.Simulator.lookup(label: str) -> int | None

Looks up the address of a label, returning None if not present.

### core.Simulator.reverse_lookup(addr: int) -> str | None

Looks up the label at a given address, returning None if there is no label.

### core.Simulator.add_breakpoint(break_loc: int | str) -> bool

Adds a breakpoint at a given location (label or address).

On next execution, if the simulator passes this location, execution pauses.

### core.Simulator.remove_breakpoint(break_loc: int | str) -> bool

Removes the breakpoint at a given location (label or address).

### core.Simulator.breakpoints (readonly property)

Readonly property which provides the current list of addresses that have a breakpoint bound to them.

### core.Simulator.{n, z, p} (readonly properties)

Readonly properties holding whether each condition code is true or not.

### core.Simulator.pc (property)

Property holding the current value of the PC.

### core.Simulator.use_real_halt (property)

A configuration setting determining whether or not HALT is "real":

- **real HALT**: HALT calls the trap handler in the OS
- **virtual HALT**: HALT immediately halts the simulator without going through the OS

### core.Simulator.strict_mem_accesses (property)

A configuration setting determining whether there should be runtime checks for invalid memory states
(notably, reads and writes of uninitialized memory)

### core.Simulator.{input, output} (properties)

Properties holding the console input and output. These can be read and written to.

### core.Simulator.frame_number (readonly property)

Readonly property holding the current frame number (number of calls deep) the simulator currently is.

### core.Simulator.frames (readonly property)

Readonly property holding the current frame stack (or `None` if `debug_frames` is disabled)

### core.Simulator.last_frame (readonly property)

Readonly property holding the last frame in the frame stack (or None if `debug_frames` is disabled)

### core.Simulator.get_subroutine_def(loc: int | str) -> SubroutineDef | None

Accesses the subroutine definition at a given memory location (label or address).

### core.Simulator.set_subroutine_def(loc: int | str, defn: SubroutineDef)

Sets the subroutine definition of a subroutine at a given memory location (label or address).

### core.Simulator.hit_halt() -> bool

Provides whether the last execution paused due to a halt.

### core.Simulator.hit_breakpoint() -> bool

Provides whether the last execution paused due to a breakpoint.
