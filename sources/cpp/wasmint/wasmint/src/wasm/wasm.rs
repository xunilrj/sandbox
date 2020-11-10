use crate::utils::*;
use super::{*, entries::*, wasm_error::*};

#[derive(Debug)]
pub enum OpCode
{
    Call(u32),
    Drop,
    I32Const(u32),
    F32Const(f32),
    End,
    Error,
}
impl OpCode
{
    pub fn from<T: std::io::Read>(f: &mut T) -> Result<OpCode, WasmError>
    {
        let opcode = read_u8(f)
            .map_err(WasmError::IOError)?;
        match opcode {
            0x0b => {
                Ok(OpCode::End)
            },
            0x10 => {
                let (idx,_) = read_uleb128(f)
                    .map_err(WasmError::IOError)?;
                Ok(OpCode::Call(idx))
            },
            0x1A => {
                Ok(OpCode::Drop)
            },
            0x41 => {
                let (v,_) = read_uleb128(f)
                    .map_err(WasmError::IOError)?;
                Ok(OpCode::I32Const(v))
            },
            0x43 => {                
                let v = read_f32(f)
                    .map_err(WasmError::IOError)?;
                Ok(OpCode::F32Const(v))
            }
            _ => Ok(OpCode::Error)
        }
    }
}


#[derive(Debug)]
pub struct FunctionBody
{
    pub instructions: Vec<OpCode>,
}
impl FunctionBody
{
    pub fn from<T: std::io::Read>(f: &mut T) -> Result<Self, WasmError>
    {
        let mut body = FunctionBody{
            instructions: Vec::new()
        };

        loop {
            match OpCode::from(f) {
                Ok(OpCode::End) => {
                    body.instructions.push(OpCode::End);
                    break;
                },
                Ok(x) => {
                    body.instructions.push(x);
                },
                Ok(OpCode::Error) => return Err(WasmError::InvalidOpcode),
                _ => return Err(WasmError::InvalidOpcode),
            }
        };

        Ok(body)
    }
}


#[repr(C)]
#[derive(Debug)]
pub enum DataTypes
{
    I32 = 0x7f,
    I64 = 0x7e,
    F32 = 0x7d,
    F64 = 0x7c,
    AnyFunc = 0x70,
    Func = 0x60,
    EmptyBlockType = 0x40,
}

impl DataTypes
{
    pub fn from<T: std::io::Read>(f: &mut T) -> Result<DataTypes, WasmError>
    {
        let (param_types,_) = read_uleb128(f)
                .map_err(WasmError::IOError)?;
        Ok(unsafe { std::mem::transmute::<u32, DataTypes>(param_types) })
    }
}

#[derive(Debug)]
pub struct FuncMetadata
{
    func_type: DataTypes,
    parameters: Vec<DataTypes>,
    pub returns: Vec<DataTypes>,
}

impl FuncMetadata
{
    pub fn from<T: std::io::Read>(f: &mut T) -> Result<Self, WasmError>
    {
        let form = DataTypes::from(f)?;
    
        let mut parameters = Vec::new();
        let (mut param_count,_) = read_uleb128(f)
            .map_err(WasmError::IOError)?;
        while param_count > 0 {
            parameters.push(DataTypes::from(f)?);
            param_count -= 1;
        }

        let mut returns = Vec::new();
        let (mut return_count,_) = read_uleb128(f)
            .map_err(WasmError::IOError)?;
        while return_count > 0 {
            returns.push(DataTypes::from(f)?);
            return_count -= 1;
        }

        let ft = FuncMetadata
        {
            func_type: form,
            parameters: parameters,
            returns: returns,
        };

        Ok(ft)    
    }
}


#[derive(PartialEq)]
pub enum ModuleHeaderValidationError
{
    InvalidMagicNumber,
    InvalidVersion
}

// https://github.com/WebAssembly/design/blob/master/BinaryEncoding.md#high-level-structure
#[repr(C)]
#[derive(Debug)]
pub struct WasmModuleHeader([u8;4], u32);

impl WasmModuleHeader
{
    pub fn is_valid(&self) -> Result<(), ModuleHeaderValidationError>
    {
        //00 61 73 6d
        if self.0[0] != 0x00 { return Err(ModuleHeaderValidationError::InvalidMagicNumber); }
        if self.0[1] != 0x61 { return Err(ModuleHeaderValidationError::InvalidMagicNumber); }
        if self.0[2] != 0x73 { return Err(ModuleHeaderValidationError::InvalidMagicNumber); }
        if self.0[3] != 0x6d { return Err(ModuleHeaderValidationError::InvalidMagicNumber); }

        if  self.1 != 1  { return Err(ModuleHeaderValidationError::InvalidVersion); }

        Ok(())
    }
}

pub struct WasmMemory
{
    mem: Vec<u8>,
    max: usize
}

impl std::fmt::Debug for WasmMemory
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error>
    {
        f.write_str("WasmMemory {}");
        Ok(())
    }
}

impl WasmMemory
{
    pub fn new(entry: &MemoryEntry) -> Self
    {
        let mem = WasmMemory {
            mem: vec![0;(entry.initial * (64 * 1024)) as usize],
            max: entry.maximum.unwrap_or(1 * 1024 * 1024) as usize
        };

        mem
    }

    pub fn at<T>(&self, addr: usize) -> *const T
    {
        &self.mem[addr] as * const u8 as *const T
    }
}

#[derive(Debug)]
pub enum WasmFunctionIndex
{
    Function(usize),
    Import(usize),
    Export(usize)
}

#[derive(Debug)]
pub struct WasmModule
{
    pub header: WasmModuleHeader,
    sections: Vec<WasmSection>,
    
    types_header: Vec<FuncMetadata>,
    functions_header: Vec<FunctionEntry>,
    bodies_header: Vec<FunctionBodyEntry>,
    pub import_header: Vec<ImportEntry>,
    pub export_header: Vec<ExportEntry>,
    pub memory_header: Vec<MemoryEntry>,

    pub functions: Vec<WasmFunctionIndex>,
    threads: Vec<WasmThreadInternal>,

    pub memories: Vec<WasmMemory>
}

#[derive(Debug)]
enum WasmSection
{
    KnownSection(usize, usize,Vec<u8>),
    TypeSection(Vec<FuncMetadata>),
    ImportSection(Vec<ImportEntry>),
    FunctionSection(Vec<FunctionEntry>),
    TableSection(Vec<TableEntry>),
    MemorySection(Vec<MemoryEntry>),
    GlobalSection(Vec<GlobalEntry>),
    ExportSection(Vec<ExportEntry>),
    FunctionBodySection(Vec<FunctionBodyEntry>),
    DataSection(Vec<DataEntry>),
    UnkownSection(usize,String,Vec<u8>)
}

#[derive(Debug)]
pub struct WasmThread
{
    id: usize,
}

#[derive(Debug)]
pub enum StackValue
{
    F32(f32),
    F64(f64),
    I32(i32),
    I64(i64),
}
impl StackValue{
    pub fn as_usize(&self) -> usize 
    {
        match self {
            StackValue::F32(x) => *x as usize,
            StackValue::F64(x) => *x as usize,
            StackValue::I32(x) => *x as usize,
            StackValue::I64(x) => *x as usize,
        }
    }

    pub fn as_ptr<T>(&self) -> *const T 
    {
        match self {
            StackValue::I32(x) => *x as *const () as *const T,
            StackValue::I64(x) => *x as *const () as *const T,
            _ => std::ptr::null()
        }
    }
}

pub fn as_str_size_ptr<'a>(stack: &'a Vec<StackValue>, mem: &WasmMemory) -> Option<&'a str>
{
    let addr = stack.get(0).unwrap().as_usize();
    let len = stack.get(1).unwrap().as_usize();

    let v = unsafe {std::slice::from_raw_parts(mem.at(addr), len) };
    Some(unsafe {std::str::from_utf8_unchecked(v)})
}


#[derive(Debug)]
struct WasmThreadInternal
{
    id: usize,
    fstack: std::vec::Vec<(usize,usize)>,
    vstack: std::vec::Vec<StackValue>
}

#[derive(Debug)]
pub enum StepResult
{
    None,
    Result(Vec<StackValue>),
    CallFunction(usize,Vec<StackValue>)
}

impl WasmModule
{
    pub fn from<TRead: std::io::Read>(f: &mut TRead) -> Result<Self, WasmError>
    {
        let mut m = WasmModule
        {
            header: read_and_cast::<WasmModuleHeader, TRead>(f)
                .map_err(WasmError::IOError)?,
            sections: Vec::new(),
            threads: Vec::new(),

            types_header: Vec::new(),
            functions_header: Vec::new(),
            bodies_header: Vec::new(),
            import_header: Vec::new(),
            export_header: Vec::new(),
            memory_header: Vec::new(),

            functions: Vec::new(),
            memories: Vec::new()
        };

        let mut cont = true;
        while cont
        {
            cont = match WasmModule::read_section(f)
            {
                Ok(WasmSection::TypeSection(v)) => { m.types_header = v; true },
                Ok(WasmSection::FunctionSection(v)) => {
                    m.functions_header = v; 
                    true
                },
                Ok(WasmSection::ImportSection(v)) => {
                    for (i, x) in v.iter().enumerate() {
                        match x.kind {
                            ExternalKind::Function(f) => {
                                m.functions.push(WasmFunctionIndex::Import(f as usize));
                            }
                            _ => {}
                        }
                    }
                    m.import_header = v; 
                    true
                },
                Ok(WasmSection::ExportSection(v)) => {
                    for (i, x) in v.iter().enumerate() {
                        match x.kind {
                            ExternalKind::Function(f) => {
                                m.functions.push(WasmFunctionIndex::Export(f as usize));
                            }
                            _ => {}
                        }
                    }
                    m.export_header = v; 
                    true
                },
                Ok(WasmSection::FunctionBodySection(v)) => { m.bodies_header = v; true },
                Ok(WasmSection::MemorySection(v)) => {
                    for mem in &v {
                        m.memories.push(WasmMemory::new(&mem));
                    }
                    m.memory_header = v; 
                    true 
                },
                Ok(WasmSection::DataSection(v)) => {
                    for d in &v {
                        let mem = m.memories.get_mut(d.index as usize).unwrap();
                        let offset = d.get_offset().unwrap();

                        let mut s = &mut mem.mem[offset..(offset + d.data.len())];
                        s.copy_from_slice(&d.data);
                    }
                    true 
                },
                Ok(s) => { m.sections.push(s); true },
                Err(WasmError::IOError(io)) 
                    if io.kind() == std::io::ErrorKind::UnexpectedEof => { false }
                Err(e) => return Err(e)
            }
        }

        Ok(m)
    }

    fn read_section<TRead: std::io::Read>(f: &mut TRead) 
        -> Result<WasmSection, WasmError>
    {
        let mut section_type= [0u8;1];
        f.read_exact(&mut section_type).map_err(WasmError::IOError)?;
        
        match section_type[0] {
            // Type	1	Function signature declarations
            1 => {
                let (_,_) = read_uleb128(f)
                    .map_err(WasmError::IOError)?;
                vec_uleb128(f, FuncMetadata::from)
                    .map_err(WasmError::IOError)
                    .map(WasmSection::TypeSection)
            },
            // Import	2	Import declarations
            2 => {
                let (_,_) = read_uleb128(f)
                    .map_err(WasmError::IOError)?;
                vec_uleb128(f, ImportEntry::from)
                    .map_err(WasmError::IOError)
                    .map(WasmSection::ImportSection)
            },
            // Function	3	Function declarations
            3 => {
                let (_,_) = read_uleb128(f)
                    .map_err(WasmError::IOError)?;
                vec_uleb128(f, FunctionEntry::from)
                    .map_err(WasmError::IOError)
                    .map(WasmSection::FunctionSection)
            },
            // Table	4	Indirect function table and other tables
            4 =>
            {
                let (_,_) = read_uleb128(f)
                    .map_err(WasmError::IOError)?;
                vec_uleb128(f, TableEntry::from)
                    .map_err(WasmError::IOError)
                    .map(WasmSection::TableSection)
            },
            // Memory	5	Memory attributes
            5 => 
            {
                let (_,_) = read_uleb128(f)
                    .map_err(WasmError::IOError)?;
                vec_uleb128(f, MemoryEntry::from)
                    .map_err(WasmError::IOError)
                    .map(WasmSection::MemorySection)
            },
            // Global	6	Global declarations
            6 =>
            {
                let (_,_) = read_uleb128(f)
                    .map_err(WasmError::IOError)?;
                vec_uleb128(f, GlobalEntry::from)
                    .map_err(WasmError::IOError)
                    .map(WasmSection::GlobalSection)
            },
            // Export	7	Exports
            7 => {
                let (_,_) = read_uleb128(f)
                    .map_err(WasmError::IOError)?;
                vec_uleb128(f, ExportEntry::from)
                    .map_err(WasmError::IOError)
                    .map(WasmSection::ExportSection)
            }
            // Start	8	Start function declaration
            // Element	9	Elements section
            // Code	10	Function bodies (code)
            10 => {
                let (_,_) = read_uleb128(f)
                .map_err(WasmError::IOError)?;
                vec_uleb128(f, FunctionBodyEntry::from)
                    .map_err(WasmError::IOError)
                    .map(WasmSection::FunctionBodySection)
            }
            11 => 
            {
                let (_,_) = read_uleb128(f)
                .map_err(WasmError::IOError)?;
                vec_uleb128(f, DataEntry::from)
                    .map_err(WasmError::IOError)
                    .map(WasmSection::DataSection)
            }

            // Data	11	Data segments
            4..=11 => 
            {
                let (size,_) = read_uleb128(f)
                    .map_err(WasmError::IOError)?;

                let mut buffer = vec![0u8;size as usize];
                f.read_exact(&mut buffer).map_err(WasmError::IOError)?;

                Ok(WasmSection::KnownSection(section_type[0] as usize, size as usize, buffer))
            }
            //default
            _ => {
                let (size, _) = read_uleb128(f)
                    .map_err(WasmError::IOError)?;

                let (name, name_size_len) = read_uleb128_string(f).map_err(WasmError::IOError)?;

                let mut buffer = vec![0u8;size as usize - name.len() - name_size_len as usize];
                f.read_exact(&mut buffer).map_err(WasmError::IOError)?;

                Ok(WasmSection::UnkownSection(size as usize, name, buffer))
            }
        }
    }

    pub fn new_thread(&mut self, fidx: usize) -> WasmThread
    {
        let id = self.threads.len();
        
        let internal = WasmThreadInternal {
            id,
            fstack: vec![(fidx,0)],
            vstack: Vec::new(),
        };
        
        self.threads.push(internal);

        WasmThread { id }
    }

    fn collect_vstack(t: &mut WasmThreadInternal, mut needed: usize) -> Option<Vec<StackValue>>
    {
        let l = t.vstack.len();
        let mut r = Vec::new();
        
        if l >= needed {
            while needed > 0
            {
                r.push(t.vstack.pop().unwrap());
                needed -= 1;
            }

            Some(r)
        } else {
            None
        }
    }

    pub fn step_instruction(&mut self, t: &WasmThread) -> Result<StepResult, WasmError>
    {
        match self.threads.get_mut(t.id) {
            Some(t) => {
                let (findex, ir) = t.fstack.last_mut().unwrap().clone();

                let fentry = self.functions_header.get(findex).unwrap();
                let fmeta = self.types_header.get(fentry.tidx as usize).unwrap();
                let function = self.bodies_header.get(findex).unwrap();
                
                match function.get_instruction(ir).unwrap() {
                    OpCode::I32Const(v) => {
                        t.vstack.push(StackValue::I32(*v as i32));
                    },
                    OpCode::F32Const(v) => {
                        t.vstack.push(StackValue::F32(*v));
                    },
                    OpCode::Call(idx) => {
                        let r = match self.functions.get(*idx as usize) {
                            Some(WasmFunctionIndex::Import(iidx)) => {
                                let ie = self.import_header.get(*iidx).unwrap();
                                let tidx = ie.get_function_type().unwrap();
                                let fmet = self.types_header.get(tidx).unwrap();
                                let r = Self::collect_vstack(t, fmet.parameters.len()).unwrap();
                                Ok(StepResult::CallFunction(*idx as usize, r))
                            },
                            _ => Err(WasmError::NotImplemented)
                        };

                        t.fstack.last_mut().unwrap().1 += 1;

                        return r;
                    },
                    OpCode::Drop => {
                        t.vstack.pop();
                    },
                    OpCode::End => {
                        let mut needed = fmeta.returns.len();
                        let l = t.vstack.len();
                        if l >= needed {
                            let mut r = Vec::new();

                            while needed > 0
                            {
                                r.push(t.vstack.pop().unwrap());
                                needed -= 1;
                            }

                            return Ok(StepResult::Result(r));
                        } 
                        return Err(WasmError::NotImplemented)                   
                    },
                    _ => return Err(WasmError::NotImplemented)
                }

                t.fstack.last_mut().unwrap().1 += 1;

                Ok(StepResult::None)
            }
            None => Err(WasmError::InvalidThread)
        }
    }

    // This method does not step
    pub fn push_result(&mut self, t: &WasmThread, values: Vec<StackValue>) -> Result<(), WasmError>
    {
        match self.threads.get_mut(t.id) {
            Some(t) => {
                for v in values {
                    t.vstack.push(v);
                }
                Ok(())
            }
            None => Err(WasmError::InvalidThread)
        }        
    }

    pub fn get_function_name(&self, idx: usize) -> Option<&str>
    {
        match self.functions.get(idx) {
            Some(WasmFunctionIndex::Import(idx)) => 
                self.import_header.get(*idx).map(|x| x.field.as_str()),
            Some(WasmFunctionIndex::Export(idx)) => 
                self.export_header.get(*idx).map(|x| x.field.as_str()),
            _ => None
        }
    }

    pub fn get_stack(&self, t: &WasmThread) -> Option<&Vec<StackValue>>
    {
        match self.threads.get(t.id) {
            Some(t) => Some(&t.vstack),
            _ => None
        }
    }

    pub fn get_stack_mut(&mut self, t: &WasmThread) -> Option<&mut Vec<StackValue>>
    {
        match self.threads.get_mut(t.id) {
            Some(t) => Some(&mut t.vstack),
            _ => None
        }
    }

    pub fn save_thread<T: std::io::Write>(&self, t: &WasmThread, w: &mut T) -> Result<(), WasmError>
    {
        match self.threads.get(t.id) {
            Some(t) => 
            {
                w.write_all(&t.id.to_le_bytes());

                w.write_all(&t.fstack.len().to_le_bytes());
                for (fidx, instr) in &t.fstack {
                    w.write_all(&fidx.to_le_bytes());
                    w.write_all(&instr.to_le_bytes());
                }

                w.write_all(&t.vstack.len().to_le_bytes());
                for v in &t.vstack {
                    match v {
                        StackValue::I32(v) => {
                            w.write(&[0]);
                            w.write_all(&v.to_le_bytes());
                        }
                        StackValue::I64(v) => {
                            w.write(&[1]);
                            w.write_all(&v.to_le_bytes());
                        }
                        StackValue::F32(v) => {
                            w.write(&[2]);
                            w.write_all(&v.to_le_bytes());
                        }
                        StackValue::F64(v) => {
                            w.write(&[3]);
                            w.write_all(&v.to_le_bytes());
                        }
                    }                    
                }
                
                Ok(())
            },
            _ => Err(WasmError::NotImplemented)
        }
        
    }
}