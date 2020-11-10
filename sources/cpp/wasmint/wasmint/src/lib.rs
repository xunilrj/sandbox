#![feature(box_syntax)]

use std::result::Result;

mod utils;
mod wasm;

use crate::wasm::*;

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;

    fn function_name(m: &WasmModule, f: &WasmFunctionIndex) -> String
    {
        match f {
            WasmFunctionIndex::Function(i) => {
                format!("{:?}", m.functions.get(*i))
            }
            WasmFunctionIndex::Import(i ) => {
                format!("{:?}", m.import_header.get(*i))
            }
            WasmFunctionIndex::Export(i ) => {
                format!("{:?}", m.export_header.get(*i))
            }
        }
    }

    #[test]
    fn it_works() -> Result<(),()> {
        let path = "/mnt/d/github/sandbox/sources/cpp/wasmint/script.c.wasm";
        let mut f = std::fs::File::open(path)
            .or(Err(()))?;
        let mut m = WasmModule::from(&mut f)
            .or(Err(()))?;
        let t0 = m.new_thread(0);

        let mut functions: Vec<Box<dyn Fn(&mut WasmModule, &WasmThread, &Vec<StackValue>)>> = Vec::new();
        functions.push(box |m, _, args| {
            let mem = m.memories.get(0).unwrap();
            let s = as_str_size_ptr(&args, mem).unwrap();
            print!("{}", s);
        });
        functions.push(box |m, t, args| {
            println!("WalkTo {:?}", args);
            m.push_result(t, vec![StackValue::I32(1)]);
        });
        functions.push(box |m, t, args| {
            println!("Wait {:?}", args);
            m.push_result(t, vec![StackValue::I32(1)]);
        });
        functions.push(box |m, t, args| {
            println!("StartTalk {:?}", args);
            m.push_result(t, vec![StackValue::I32(1)]);
        });

        loop {
            match m.step_instruction(&t0) {
                Ok(StepResult::CallFunction(r, args)) => {
                    match functions.get(r) 
                    {
                        Some(f) => f(&mut m, &t0, &args),
                        None => {
                            println!("Missing method: {:?}", function_name(&m, m.functions.get(r).unwrap()));
                        }
                    }
                },
                Ok(StepResult::Result(r)) => {
                    println!("Result: {:?}", r);
                    break;
                }
                _ => {}
            }
        }

        dbg!(m.step_instruction(&t0));
        dbg!(m.step_instruction(&t0));
        dbg!(m.step_instruction(&t0));
        if let StepResult::CallFunction(r, _) = dbg!(m.step_instruction(&t0)).unwrap() {
            println!("Calling: {:?}", m.get_function_name(r));
        }
        m.push_result(&t0, vec![StackValue::I64(0)]);
        if let StepResult::CallFunction(r, _) = dbg!(m.step_instruction(&t0)).unwrap() {
            println!("Calling: {:?}", m.get_function_name(r));
        }
        
        assert!(m.header.is_valid() == Ok(()));

        Ok(())
    }
}
