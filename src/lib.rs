use lc3_ensemble::asm::assemble_debug;
use lc3_ensemble::ast::reg_consts::{R0, R1, R2, R3, R4, R5, R6, R7};
use lc3_ensemble::parse::parse_ast;
use lc3_ensemble::sim::Simulator;
use pyo3::{create_exception, prelude::*};
use pyo3::exceptions::{PyIndexError, PyValueError};

/// A LC-3 simulator and unit tester, backed by [`lc3-ensemble`].
#[pymodule]
fn ensemble_test(py: Python, m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<PySimulator>()?;
    m.add("LoadError", py.get_type_bound::<LoadError>())?;
    m.add("SimError", py.get_type_bound::<SimError>())?;
    
    Ok(())
}

create_exception!(ensemble_test, LoadError, PyValueError);
create_exception!(ensemble_test, SimError, PyValueError);

impl LoadError {
    fn from_lc3_err(e: impl lc3_ensemble::err::Error) -> PyErr {
        struct ErrDisplay<'e, E>(&'e E);
        impl<E: lc3_ensemble::err::Error> std::fmt::Display for ErrDisplay<'_, E> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                std::fmt::Display::fmt(self.0, f)?;
                if let Some(span) = self.0.span() {
                    write!(f, "({span:?})")?;
                }
                Ok(())
            }
        }
        
        LoadError::new_err(ErrDisplay(&e).to_string())
    }
}

#[derive(FromPyObject)]
enum BreakpointLocation {
    Address(u16),
    Label(String)
}
#[pyclass(name="Simulator")]
struct PySimulator {
    sim: Simulator,
}

#[pymethods]
impl PySimulator {
    #[new]
    fn constructor() -> Self {
        PySimulator { sim: Simulator::new(Default::default()) }
    }

    fn load_file(&mut self, src_fp: &str) -> PyResult<()> {
        todo!()
    }
    fn load_code(&mut self, src: &str) -> PyResult<()> {
        todo!()
    }

    fn run(&mut self, limit: Option<u64>) -> PyResult<()> {
        todo!()
    }
    fn step_in(&mut self) -> PyResult<()> {
        todo!()
    }
    fn step_out(&mut self) -> PyResult<()> {
        todo!()
    }
    fn step_over(&mut self) -> PyResult<()> {
        todo!()
    }

    fn read_mem(&mut self, addr: u16) -> PyResult<u16> {
        todo!()
    }
    fn write_mem(&mut self, addr: u16, val: u16) -> PyResult<()> {
        todo!()
    }
    fn get_mem(&self, addr: u16) -> u16 {
        todo!()
    }
    fn set_mem(&mut self, addr: u16, val: u16) {
        todo!()
    }

    #[getter]
    fn get_r0(&self) -> u16 {
        todo!()
    }
    #[setter]
    fn set_r0(&mut self, value: u16) {
        todo!()
    }
    #[getter]
    fn get_r1(&self) -> u16 {
        todo!()
    }
    #[setter]
    fn set_r1(&mut self, value: u16) {
        todo!()
    }
    #[getter]
    fn get_r2(&self) -> u16 {
        todo!()
    }
    #[setter]
    fn set_r2(&mut self, value: u16) {
        todo!()
    }
    #[getter]
    fn get_r3(&self) -> u16 {
        todo!()
    }
    #[setter]
    fn set_r3(&mut self, value: u16) {
        todo!()
    }
    #[getter]
    fn get_r4(&self) -> u16 {
        todo!()
    }
    #[setter]
    fn set_r4(&mut self, value: u16) {
        todo!()
    }
    #[getter]
    fn get_r5(&self) -> u16 {
        todo!()
    }
    #[setter]
    fn set_r5(&mut self, value: u16) {
        todo!()
    }
    #[getter]
    fn get_r6(&self) -> u16 {
        todo!()
    }
    #[setter]
    fn set_r6(&mut self, value: u16) {
        todo!()
    }
    #[getter]
    fn get_r7(&self) -> u16 {
        todo!()
    }
    #[setter]
    fn set_r7(&mut self, value: u16) {
        todo!()
    }
    fn get_reg(&self, index: usize) -> PyResult<u16> {
        todo!()
    }
    fn set_reg(&mut self, index: usize, val: u16) -> PyResult<()> {
        todo!()
    }

    fn lookup(&self, label: &str) -> Option<u16> {
        todo!()
    }
    fn reverse_lookup(&self, addr: u16) -> Option<&str> {
        todo!()
    }
    fn add_label(&mut self, label: &str, addr: u16) -> bool {
        todo!()
    }
    fn delete_label(&mut self, label: &str) {
        todo!()
    }

    fn add_breakpoint(&mut self, break_loc: BreakpointLocation) {
        todo!()
    }
    fn remove_breakpoint(&mut self, break_loc: BreakpointLocation) {
        todo!()
    }
    
    fn breakpoints(&self) -> Vec<u16> {
        todo!()
    }

    #[getter]
    fn get_n(&self) -> bool {
        todo!()
    }
    #[getter]
    fn get_z(&self) -> bool {
        todo!()
    }
    #[getter]
    fn get_p(&self) -> bool {
        todo!()
    }

    #[getter]
    fn get_pc(&self) -> u16 {
        todo!()
    }
    #[setter]
    fn set_pc(&mut self, addr: u16) {
        todo!()
    }

    #[getter]
    fn get_executions(&self) -> u64 {
        todo!()
    }

    #[getter]
    fn get_use_real_halt(&self) -> bool {
        todo!()
    }
    #[setter]
    fn set_use_real_halt(&mut self, status: bool) {
        todo!()
    }
    
    #[getter]
    fn get_strict_execution(&self) -> bool {
        todo!()
    }
    #[setter]
    fn set_strict_execution(&mut self, status: bool) {
        todo!()
    }
    
    #[getter]
    fn get_input(&self) -> &str {
        todo!()
    }
    #[setter]
    fn set_input(&mut self, input: &str) {
        todo!()
    }
    
    #[getter]
    fn get_output(&self) -> &str {
        todo!()
    }
    #[setter]
    fn set_output(&mut self, output: &str) {
        todo!()
    }

    // fn init(&mut self, src_fp: &str) -> PyResult<()> {
    //     self.sim = Simulator::new(Default::default());
        
    //     let src = std::fs::read_to_string(src_fp)?;
    //     let ast = parse_ast(&src)
    //         .map_err(LoadError::from_lc3_err)?;
    //     let obj = assemble_debug(ast, &src)
    //         .map_err(LoadError::from_lc3_err)?;

    //     self.sim.load_obj_file(&obj);
    //     Ok(())
    // }

    // fn run(&mut self) -> PyResult<()> {
    //     self.sim.run()
    //         .map_err(|e| SimError::new_err(format!("{e} (PC: 0x{})", self.sim.prefetch_pc())))
    // }

    // fn get_reg(&self, reg: i32) -> PyResult<u16> {
    //     match reg {
    //         0 => Ok(self.sim.reg_file[R0].get()),
    //         1 => Ok(self.sim.reg_file[R1].get()),
    //         2 => Ok(self.sim.reg_file[R2].get()),
    //         3 => Ok(self.sim.reg_file[R3].get()),
    //         4 => Ok(self.sim.reg_file[R4].get()),
    //         5 => Ok(self.sim.reg_file[R5].get()),
    //         6 => Ok(self.sim.reg_file[R6].get()),
    //         7 => Ok(self.sim.reg_file[R7].get()),
    //         _ => Err(PyErr::new::<PyIndexError, _>("Invalid Register Specified"))
    //     }
    // }

    // fn get_memory(&mut self, address: u16) -> PyResult<u16>{
    //     Ok(self.sim.mem.get_raw(address).get())
    // }
}