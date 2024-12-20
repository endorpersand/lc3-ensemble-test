use std::path::PathBuf;

use lc3_ensemble::asm::{assemble_debug, ObjectFile, SourceInfo};
use lc3_ensemble::ast::Reg::{R0, R1, R2, R3, R4, R5, R6, R7};
use lc3_ensemble::ast::Reg;
use lc3_ensemble::parse::parse_ast;
use lc3_ensemble::sim::debug::Breakpoint;
use lc3_ensemble::sim::device::{BufferedDisplay, BufferedKeyboard, Interrupt, InterruptFromFn};
use lc3_ensemble::sim::frame::{Frame, ParameterList};
use lc3_ensemble::sim::mem::{MachineInitStrategy, Word};
use lc3_ensemble::sim::{MemAccessCtx, SimErr, SimFlags, Simulator};
use pyo3::types::PyInt;
use pyo3::{create_exception, prelude::*};
use pyo3::exceptions::{PyIndexError, PyTypeError, PyValueError};

/// Bindings for the LC3 simulator.
#[pymodule]
fn ensemble_test(py: Python, m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<PySimulator>()?;
    m.add("LoadError", py.get_type::<LoadError>())?;
    m.add("SimError", py.get_type::<SimError>())?;
    m.add_class::<MemoryFillType>()?;
    m.add_class::<CallingConventionSRDef>()?;
    m.add_class::<PassByRegisterSRDef>()?;
    m.add_class::<PyFrame>()?;

    Ok(())
}

create_exception!(ensemble_test, LoadError, PyValueError);
create_exception!(ensemble_test, SimError, PyValueError);

impl LoadError {
    fn from_lc3_err(e: impl lc3_ensemble::err::Error, src: &str) -> PyErr {
        use std::ops::Range;

        struct ErrDisplay<'e, E>(&'e E, &'e str);
        impl<E: lc3_ensemble::err::Error> std::fmt::Display for ErrDisplay<'_, E> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let src_info = SourceInfo::new(self.1);

                std::fmt::Display::fmt(self.0, f)?;
                if let Some(span) = self.0.span() {
                    write!(f, " (")?;
                    let mut it = span.iter().map(|&Range { start, end: _ }| src_info.get_pos_pair(start));
                    if let Some((flno, fcno)) = it.next() {
                        write!(f, "line {}, col {}", flno + 1, fcno + 1)?;
                        for (lno, cno) in it {
                            write!(f, "; line {}, col {}", lno + 1, cno + 1)?;
                        }
                    }
                    writeln!(f, ")")?;
                    
                    for &Range { start, end: _ } in span.iter() {
                        let (lno, _) = src_info.get_pos_pair(start);
                        writeln!(f, "line {}:", lno + 1)?;
                        writeln!(f, "  {}", src_info.read_line(lno).unwrap_or(""))?;

                    }
                }
                Ok(())
            }
        }
        
        LoadError::new_err(ErrDisplay(&e, src).to_string())
    }
}
impl SimError {
    fn from_display(err: impl std::fmt::Display, pc: u16) -> PyErr {
        SimError::new_err(format!("{err} (PC: x{pc:04X})"))
    }

    fn from_lc3_err(err: SimErr, pc: u16) -> PyErr {
        match err {
            SimErr::Interrupt(e) => match e.into_inner().downcast() {
                Ok(py_err) => *py_err,
                Err(e) => SimError::from_display(e, pc),
            },
            e => SimError::from_display(e, pc)
        }
    }
}

#[derive(FromPyObject)]
enum MemLocation {
    Address(u16),
    Label(String)
}

#[derive(Clone, Copy, PartialEq, Eq)]
#[pyclass(module="ensemble_test", eq, eq_int)]
/// Strategies to fill the memory on initializing the simulator.
enum MemoryFillType {
    /// Fill the memory with random values.
    Random,
    /// Fill the memory with a single known value.
    Single
}

#[derive(Clone, Copy)]
#[repr(transparent)]
struct RegWrapper(Reg);
impl<'py> IntoPyObject<'py> for RegWrapper {
    type Target = PyInt;
    type Output = Bound<'py, Self::Target>;
    type Error = std::convert::Infallible;

    fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
        self.0.reg_no().into_pyobject(py)
    }
}
impl<'py> FromPyObject<'py> for RegWrapper {
    fn extract_bound(ob: &Bound<'py, PyAny>) -> PyResult<Self> {
        ob.extract::<u8>().ok()
            .and_then(|i| Reg::try_from(i).ok())
            .map(RegWrapper)
            .ok_or_else(|| PyIndexError::new_err(format!("register {ob} out of bounds")))
    }
}
impl std::fmt::Debug for RegWrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
#[derive(Clone)]
#[pyclass(module="ensemble_test")]
/// Subroutine definition based on standard LC-3 calling convention.
struct CallingConventionSRDef {
    /// A list of parameter names.
    #[pyo3(get)]
    params: Vec<String>
}
#[pymethods]
impl CallingConventionSRDef {
    #[new]
    fn constructor(params: Vec<String>) -> Self {
        Self { params }
    }

    fn __repr__(&self) -> String {
        format!("CallingConventionSRDef(params={:?})", self.params)
    }
}
#[derive(Clone)]
#[pyclass(module="ensemble_test")]
/// Subroutine definition based on pass-by-register calling convention.
struct PassByRegisterSRDef {
    /// A list of parameter names and associated register per parameter.
    #[pyo3(get)]
    params: Vec<(String, RegWrapper)>,
    /// The return register (if present).
    #[pyo3(get)]
    ret: Option<RegWrapper>
}
#[pymethods]
impl PassByRegisterSRDef {
    #[new]
    #[pyo3(signature=(params, ret = None))]
    fn constructor(params: Vec<(String, RegWrapper)>, ret: Option<RegWrapper>) -> Self {
        Self { params, ret }
    }

    fn __repr__(&self) -> String {
        format!("PassByRegisterSRDef(params={:?}, ret={:?})", self.params, self.ret)
    }
}

#[repr(transparent)]
struct PyParamListWrapper(ParameterList);
impl<'py> IntoPyObject<'py> for PyParamListWrapper {
    type Target = PyAny;
    type Output = Bound<'py, Self::Target>;
    type Error = PyErr;
    
    fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
        match self.0 {
            ParameterList::CallingConvention { params } => {
                CallingConventionSRDef { params }.into_pyobject(py)
                    .map(Bound::into_any)
            },
            ParameterList::PassByRegister { params, ret } => {
                let params: Vec<_> = params.into_iter()
                    .map(|(s, r)| (s, RegWrapper(r)))
                    .collect();
                let ret = ret.map(RegWrapper);

                PassByRegisterSRDef { params, ret }.into_pyobject(py)
                    .map(Bound::into_any)
            },
        }
    }
}
impl<'py> FromPyObject<'py> for PyParamListWrapper {
    fn extract_bound(ob: &Bound<'py, PyAny>) -> PyResult<Self> {
        if let Ok(CallingConventionSRDef { params }) = ob.extract() {
            Ok(Self(ParameterList::CallingConvention { params }))
        } else if let Ok(PassByRegisterSRDef { params, ret }) = ob.extract() {
            let params = params.into_iter().map(|(s, rw)| (s, rw.0)).collect();
            let ret = ret.map(|rw| rw.0);
            Ok(Self(ParameterList::PassByRegister { params, ret }))
        } else {
            let err_msg = format!(
                "failed to convert the value to 'Union[{}, {}]'",
                std::any::type_name::<CallingConventionSRDef>(),
                std::any::type_name::<PassByRegisterSRDef>()
            );
            Err(PyTypeError::new_err(err_msg))
        }
    }
}

#[repr(transparent)]
#[pyclass(name="Frame", module="ensemble_test")]
struct PyFrame(Frame);

#[pymethods]
impl PyFrame {
    #[getter]
    fn get_caller_addr(&self) -> u16 {
        self.0.caller_addr
    }
    #[getter]
    fn get_callee_addr(&self) -> u16 {
        self.0.callee_addr
    }
    #[getter]
    fn get_frame_type(&self) -> u16 {
        self.0.frame_type as u16
    }
    #[getter]
    fn get_frame_ptr(&self) -> Option<(u16, bool)> {
        let word = self.0.frame_ptr?;
        Some((word.get(), word.is_init()))
    }
    #[getter]
    fn get_arguments(&self) -> Vec<(u16, bool)> {
        self.0.arguments
            .iter()
            .map(|w| (w.get(), w.is_init()))
            .collect()
    }
    fn __repr__(&self) -> String {
        format!(
            "Frame {{ caller_addr: {caller_addr}, callee_addr: {callee_addr}, frame_type: {frame_type}, frame_ptr: {frame_ptr:?}, arguments: {arguments:?} }}",
            caller_addr = self.get_caller_addr(),
            callee_addr = self.get_callee_addr(),
            frame_type = self.get_frame_type(),
            frame_ptr = self.get_frame_ptr(),
            arguments = self.get_arguments(),
        )
    }
}

/// The simulator!
#[pyclass(name="Simulator", module="ensemble_test")]
struct PySimulator {
    sim: Simulator,
    obj: Option<ObjectFile>,
    input: BufferedKeyboard,
    output: BufferedDisplay
}

impl PySimulator {
    fn reset(&mut self) {
        self.sim.reset();

        self.obj.take();

        self.input.get_buffer().write().unwrap_or_else(|e| e.into_inner()).clear();
        self.output.get_buffer().write().unwrap_or_else(|e| e.into_inner()).clear();
    }

    fn resolve_location(&self, loc: MemLocation) -> Result<u16, String> {
        match loc {
            MemLocation::Address(addr) => Ok(addr),
            MemLocation::Label(label) => self.lookup(&label).ok_or(label),
        }
    }
}
#[pymethods]
impl PySimulator {
    #[new]
    fn constructor() -> Self {
        let flags = SimFlags {
            debug_frames: true,
            ..Default::default()
        };
        
        let mut this = Self {
            sim: Simulator::new(flags),
            obj: None,
            input: Default::default(),
            output: Default::default()
        };

        // Set IO:
        this.sim.device_handler.set_keyboard(this.input.clone());
        this.sim.device_handler.set_display(this.output.clone());

        let int_handler = InterruptFromFn::new(|| {
            Python::with_gil(|py| py.check_signals())
                .err()
                .map(Interrupt::external)
        });
        this.sim.device_handler.add_device(int_handler, &[])
            .unwrap_or_else(|_| panic!("Could not add interrupt handler device"));

        this.reset();
        this
    }

    /// Initialize the register files and memory of the simulator with the provided fill type and seed.
    /// 
    /// The following argument patterns are allowed:
    /// - `(MemoryFillType.Random, None)` -> randomly fill memory, with an arbitrary seed
    /// - `(MemoryFillType.Random, int)`  -> randomly fill memory, with the provided seed
    /// - `(MemoryFillType.Single, None)` -> fill memory with 0
    /// - `(MemoryFillType.Single, int)`  -> fill memory with the provided value
    /// 
    /// This method returns the seed/value that is used to initialize the simulator.
    #[pyo3(signature=(fill, value = None))]
    fn init(&mut self, fill: MemoryFillType, value: Option<u64>) -> u64 {
        let (strat, ret_value) = match fill {
            MemoryFillType::Random => {
                let seed = value.unwrap_or_else(rand::random);
                (MachineInitStrategy::Seeded { seed }, seed)
            },
            MemoryFillType::Single => {
                let value = value.unwrap_or(0);
                (MachineInitStrategy::Known { value: value as u16 }, value)
            },
        };
        
        self.sim.flags.machine_init = strat;
        self.reset();
        ret_value
    }

    /// Loads ASM code from a file, assembles it, 
    /// and loads the resulting object file into the simulator.
    /// 
    /// This can raise a [`LoadError`] if assembling fails.
    fn load_file(&mut self, src_fp: PathBuf) -> PyResult<()> {
        let src = std::fs::read_to_string(src_fp)?;
        self.load_code(&src)
    }

    /// Assembles ASM code from a provided string, 
    /// and loads the resulting object file into the simulator.
    /// 
    /// This can raise a [`LoadError`] if assembling fails.
    fn load_code(&mut self, src: &str) -> PyResult<()> {
        self.reset();

        let ast = parse_ast(src)
            .map_err(|e| LoadError::from_lc3_err(e, src))?;
        let obj = assemble_debug(ast, src)
            .map_err(|e| LoadError::from_lc3_err(e, src))?;
        
        self.sim.load_obj_file(&obj)
            .map_err(|e| LoadError::new_err(format!("failed to load object file: {e}")))?;
        self.obj.replace(obj);
        Ok(())
    }

    /// Runs the simulator.
    /// 
    /// A `limit` parameter can be specified to limit the number of executions ran.
    /// 
    /// This can raise a [`SimError`] if an error occurs while simulating.
    #[pyo3(signature=(limit = None))]
    fn run(&mut self, limit: Option<u64>) -> PyResult<()> {
        let result = if let Some(lim) = limit {
            self.sim.run_with_limit(lim)
        } else {
            self.sim.run()
        };

        result
            .map_err(|e| SimError::from_lc3_err(e, self.sim.prefetch_pc()))
    }
    /// Perform a step in.
    /// 
    /// This can raise a [`SimError`] if an error occurs while simulating.
    fn step_in(&mut self) -> PyResult<()> {
        self.sim.step_in()
            .map_err(|e| SimError::from_lc3_err(e, self.sim.prefetch_pc()))
    }
    /// Perform a step out.
    /// 
    /// This can raise a [`SimError`] if an error occurs while simulating.
    fn step_out(&mut self) -> PyResult<()> {
        self.sim.step_out()
            .map_err(|e| SimError::from_lc3_err(e, self.sim.prefetch_pc()))
    }
    /// Perform a step over.
    /// 
    /// This can raise a [`SimError`] if an error occurs while simulating.
    fn step_over(&mut self) -> PyResult<()> {
        self.sim.step_over()
            .map_err(|e| SimError::from_lc3_err(e, self.sim.prefetch_pc()))
    }
    /// Runs until a frame changes.
    /// 
    /// This is not meant for general use.
    #[pyo3(signature=(stop = None))]
    fn _run_until_frame_change(&mut self, stop: Option<u64>) -> PyResult<()> {
        let frame = self.sim.frame_stack.len();

        self.sim.run_while(|sim| match stop {
            Some(stop) => sim.frame_stack.len() == frame && sim.instructions_run < stop,
            None => sim.frame_stack.len() == frame
        })
            .map_err(|e| SimError::from_lc3_err(e, self.sim.prefetch_pc()))
    }
    
    #[pyo3(signature=(
        addr,
        *,
        privileged = true,
        strict = false
    ))]
    /// Reads a value from memory, triggering any I/O devices if applicable.
    /// 
    /// See `get_mem` if you wish to get the memory directly without triggering I/O devices.
    /// 
    /// This function also accepts optional `privileged` and `strict` parameters.
    /// These designate whether to read memory in privileged mode and with strict memory access.
    fn read_mem(&mut self, addr: u16, privileged: bool, strict: bool) -> PyResult<u16> {
        // Note: technically lc3-ensemble does accept non-effectful IO reads (vvvvvvvvvvvvvvvv)
        // but it's not reaaally necessary for AG, so I am leaving it off of
        // the Python binding unless something goes wrong down the line
        let word = self.sim.read_mem(addr, MemAccessCtx { privileged, strict, io_effects: true })
            .map_err(|e| SimError::from_lc3_err(e, self.sim.prefetch_pc()))?;

        Ok(word.get())
    }
        #[pyo3(signature=(
        addr,
        val,
        *,
        privileged = true,
        strict = false
    ))]

    /// Writes a value to memory, triggering any I/O devices if applicable.
    /// 
    /// See `set_mem` if you wish to set the memory directly without triggering I/O devices.
    /// 
    /// This function also accepts optional `privileged` and `strict` parameters.
    /// These designate whether to write memory in privileged mode and with strict memory access.
    fn write_mem(&mut self, addr: u16, val: u16, privileged: bool, strict: bool) -> PyResult<()> {
        self.sim.write_mem(addr, Word::new_init(val), MemAccessCtx { privileged, strict, io_effects: true })
            .map_err(|e| SimError::from_lc3_err(e, self.sim.prefetch_pc()))
    }

    /// Gets a given value from memory without triggering I/O devices.
    /// 
    /// This function does not activate any I/O devices (and therefore can result in incorrect I/O values).
    /// If you wish to trigger I/O devices, use `read_mem`.
    fn get_mem(&self, addr: u16) -> u16 {
        self.sim.mem[addr].get()
    }
    /// Sets a given value from memory without triggering I/O devices.
    /// 
    /// This function does not activate any I/O devices (and therefore can result in incorrect I/O values).
    /// If you wish to trigger I/O devices, use `write_mem`.
    fn set_mem(&mut self, addr: u16, val: u16) {
        self.sim.mem[addr].set(val);
    }

    /// The value of register 0.
    #[getter]
    fn get_r0(&self) -> u16 {
        self.sim.reg_file[R0].get()
    }
    #[setter]
    fn set_r0(&mut self, value: u16) {
        self.sim.reg_file[R0].set(value)
    }
    /// The value of register 1.
    #[getter]
    fn get_r1(&self) -> u16 {
        self.sim.reg_file[R1].get()
    }
    #[setter]
    fn set_r1(&mut self, value: u16) {
        self.sim.reg_file[R1].set(value)
    }
    /// The value of register 2.
    #[getter]
    fn get_r2(&self) -> u16 {
        self.sim.reg_file[R2].get()
    }
    #[setter]
    fn set_r2(&mut self, value: u16) {
        self.sim.reg_file[R2].set(value)
    }
    /// The value of register 3.
    #[getter]
    fn get_r3(&self) -> u16 {
        self.sim.reg_file[R3].get()
    }
    #[setter]
    fn set_r3(&mut self, value: u16) {
        self.sim.reg_file[R3].set(value)
    }
    /// The value of register 4.
    #[getter]
    fn get_r4(&self) -> u16 {
        self.sim.reg_file[R4].get()
    }
    #[setter]
    fn set_r4(&mut self, value: u16) {
        self.sim.reg_file[R4].set(value)
    }
    #[getter]
    /// The value of register 5.
    fn get_r5(&self) -> u16 {
        self.sim.reg_file[R5].get()
    }
    #[setter]
    fn set_r5(&mut self, value: u16) {
        self.sim.reg_file[R5].set(value)
    }
    /// The value of register 6.
    #[getter]
    fn get_r6(&self) -> u16 {
        self.sim.reg_file[R6].get()
    }
    #[setter]
    fn set_r6(&mut self, value: u16) {
        self.sim.reg_file[R6].set(value)
    }
    /// The value of register 7.
    #[getter]
    fn get_r7(&self) -> u16 {
        self.sim.reg_file[R7].get()
    }
    #[setter]
    fn set_r7(&mut self, value: u16) {
        self.sim.reg_file[R7].set(value)
    }

    /// Gets a value from a register.
    /// 
    /// This raises an error if the index is not between 0 and 7, inclusive.
    fn get_reg(&self, index: Bound<'_, PyInt>) -> PyResult<u16> {
        let reg = index.extract::<RegWrapper>()?.0;
        Ok(self.sim.reg_file[reg].get())
    }

    /// Sets a value to a register.
    /// 
    /// This raises an error if the index is not between 0 and 7, inclusive.
    fn set_reg(&mut self, index: Bound<'_, PyInt>, val: u16) -> PyResult<()> {
        let reg = index.extract::<RegWrapper>()?.0;
        self.sim.reg_file[reg].set(val);
        Ok(())
    }

    /// Looks up the address of a given label, returning None if the label is not defined.
    fn lookup(&self, label: &str) -> Option<u16> {
        self.obj.as_ref()?.symbol_table()?.lookup_label(label)
    }
    /// Looks up the label at a given address, returning None if no label is at the given address.
    fn reverse_lookup(&self, addr: u16) -> Option<&str> {
        self.obj.as_ref()?.symbol_table()?.rev_lookup_label(addr)
    }

    /// Adds a breakpoint to the given location.
    /// 
    /// This returns whether the insertion was successful.
    fn add_breakpoint(&mut self, break_loc: MemLocation) -> PyResult<bool> {
        let addr = self.resolve_location(break_loc)
            .map_err(|label| PyValueError::new_err(format!("cannot add a breakpoint at non-existent label {label:?}")))?;
            

        Ok(self.sim.breakpoints.insert(Breakpoint::PC(addr)))
    }
    /// Removes a breakpoint at the given location.
    /// 
    /// This returns whether the removal was successful (i.e., whether there is a breakpoint at the given location).
    fn remove_breakpoint(&mut self, break_loc: MemLocation) -> PyResult<bool> {
        let addr = self.resolve_location(break_loc)
        .map_err(|label| PyValueError::new_err(format!("cannot add a breakpoint at non-existent label {label:?}")))?;
        

        Ok(self.sim.breakpoints.insert(Breakpoint::PC(addr)))
    }
    
    /// Gets a list of currently defined breakpoints.
    #[getter]
    fn breakpoints(&self) -> Vec<u16> {
        self.sim.breakpoints.iter()
            .filter_map(|bpt| match *bpt {
                Breakpoint::PC(addr) => Some(addr),
                _ => None
            })
            .collect()
    }

    /// The n condition code.
    #[getter]
    fn get_n(&self) -> bool {
        self.sim.psr().is_n()
    }
    /// The z condition code.
    #[getter]
    fn get_z(&self) -> bool {
        self.sim.psr().is_z()
    }
    /// The p condition code.
    #[getter]
    fn get_p(&self) -> bool {
        self.sim.psr().is_p()
    }

    /// The program counter.
    #[getter]
    fn get_pc(&self) -> u16 {
        self.sim.pc
    }
    #[setter]
    fn set_pc(&mut self, addr: u16) {
        self.sim.pc = addr;
    }

    /// The number of instructions run since the simulator started running.
    #[getter]
    fn get_instructions_run(&self) -> u64 {
        self.sim.instructions_run
    }

    /// Configuration setting to designate whether to use real HALT or virtual HALT.
    #[getter]
    fn get_use_real_traps(&self) -> bool {
        self.sim.flags.use_real_traps
    }
    #[setter]
    fn set_use_real_traps(&mut self, status: bool) {
        self.sim.flags.use_real_traps = status;
    }
    
    /// Configuration setting to designate whether to use strict memory accesses during execution.
    #[getter]
    fn get_strict_mem_accesses(&self) -> bool {
        self.sim.flags.strict
    }
    #[setter]
    fn set_strict_mem_accesses(&mut self, status: bool) {
        self.sim.flags.strict = status;
    }
    
    /// The I/O input.
    #[getter]
    fn get_input(&self) -> String {
        let data: Vec<_> = self.input.get_buffer()
            .read()
            .unwrap_or_else(|e| e.into_inner())
            .iter()
            .copied()
            .collect();

        String::from_utf8_lossy(&data).into_owned()
    }
    #[setter]
    fn set_input(&mut self, input: &str) {
        let mut inp = self.input.get_buffer()
            .write()
            .unwrap_or_else(|e| e.into_inner());

        inp.clear();
        inp.extend(input.as_bytes());
    }
    fn append_to_input(&mut self, input: &str) {
        self.input.get_buffer()
            .write()
            .unwrap_or_else(|e| e.into_inner())
            .extend(input.as_bytes())
    }

    /// The I/O output.
    #[getter]
    fn get_output(&self) -> String {
        String::from_utf8_lossy({
            &self.output.get_buffer()
                .read()
                .unwrap_or_else(|e| e.into_inner())
        }).into_owned()
    }
    #[setter]
    fn set_output(&mut self, output: &str) {
        let mut out = self.output.get_buffer()
        .write()
        .unwrap_or_else(|e| e.into_inner());

        out.clear();
        out.extend(output.as_bytes());
    }

    // Subroutine definitions and frames!

    /// Gets the definition of the subroutine located at the provided location, or None if no definition has been made.
    /// 
    /// A definition needs to be made with the `set_subroutine_def` method in order for this method to return a
    /// non-None value.
    fn get_subroutine_def(&self, loc: MemLocation) -> PyResult<Option<PyParamListWrapper>> {
        let addr = self.resolve_location(loc)
            .map_err(|label| PyValueError::new_err(format!("cannot get subroutine at non-existent label {label:?}")))?;

        Ok({
            self.sim.frame_stack.get_subroutine_def(addr)
                .map(|pl| PyParamListWrapper(pl.clone()))
        })
    }

    /// Sets the definiton of the subroutine located at the provided location.
    fn set_subroutine_def(&mut self, loc: MemLocation, pl: PyParamListWrapper) -> PyResult<()> {
        let addr = self.resolve_location(loc)
            .map_err(|label| PyValueError::new_err(format!("cannot define subroutine at non-existent label {label:?}")))?;

        self.sim.frame_stack.set_subroutine_def(addr, pl.0);
        Ok(())
    }

    /// Calls the subroutine located at the provided location, updating the PC and return address.
    fn call_subroutine(&mut self, loc: MemLocation) -> PyResult<()> {
        let addr = self.resolve_location(loc)
            .map_err(|label| PyValueError::new_err(format!("cannot call subroutine at non-existent label {label:?}")))?;

        self.sim.call_subroutine(addr)
            .map_err(|e| SimError::from_lc3_err(e, self.sim.prefetch_pc()))
    }

    /// Gets the total number of frames entered (the number of subroutine/trap calls we're currently deep in)
    #[getter]
    fn get_frame_number(&self) -> u64 {
        self.sim.frame_stack.len()
    }

    /// Gets a list of the current frames in the frame stack.
    #[getter]
    fn get_frames(&self) -> Option<Vec<PyFrame>> {
        let frames = self.sim.frame_stack.frames()?
            .iter()
            .cloned()
            .map(PyFrame)
            .collect();

        Some(frames)
    }

    /// Gets the last frame of the frame stack.
    #[getter]
    fn get_last_frame(&self) -> Option<PyFrame> {
        self.sim.frame_stack.frames()?
            .last()
            .cloned()
            .map(PyFrame)
    }

    /// Returns true if last execution hit HALT.
    fn hit_halt(&self) -> bool {
        self.sim.hit_halt()
    }
    /// Returns true if last execution hit a breakpoint.
    fn hit_breakpoint(&self) -> bool {
        self.sim.hit_breakpoint()
    }
}