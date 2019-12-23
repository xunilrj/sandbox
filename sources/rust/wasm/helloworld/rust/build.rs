// build.rs

use std::env;
use std::fs::File;
use std::io::Write;
use std::path::Path;

use glsl::syntax::{SingleDeclaration,    
    StorageQualifier, TypeQualifierSpec };
use glsl::visitor::{Visit, Visitor};

struct Counter {
  var_nb: usize
}

impl Visitor for Counter {
    // fn visit_identifier(&mut self, id: &mut Identifier) -> Visit {
    //     print!("identifier: {}\n", id.0);
    //     Visit::Children
    // }

    fn visit_single_declaration(&mut self, decl: &mut SingleDeclaration) -> Visit {
        let qs = &decl.ty
            .qualifier
                .as_ref().unwrap()
            .qualifiers.0;
        if qs.len() < 2 { return Visit::Parent; }
        if qs[1] != TypeQualifierSpec::Storage(StorageQualifier::In) { return Visit::Parent; }
        match &decl.name {
            Some(str) => {
                print!("declaration: {:?} {}\n", qs[1], str);

                print!("let shader = gl.create_shader(t).jsok()?;\n",);
                print!("gl.shader_source(&shader, code);\n");
                print!("gl.compile_shader(&shader);\n");
    
                self.var_nb += 1;
            },
            None => {}   
        }

        Visit::Parent
    }
}



fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("hello.rs");
    let mut f = File::create(&dest_path).unwrap();

    f.write_all(b"
        pub fn message() -> &'static str {
            \"Hello, World!\"
        }
    ").unwrap();


    let vs = "attribute vec3 coordinates;
void main(void) {
    gl_Position = vec4(coordinates, 1.0);
}";
    let fs = "
void main(void) {
    gl_FragColor = vec4(1.0, 0.0, 0.0, 1);
}";

    print!("fn shader_pbr() {{\n");
        print!("let vscode = \"{}\";\n", vs);
        print!("let vsshader = gl.create_shader(t).jsok()?;\n",);
        print!("gl.shader_source(&vsshader, vscode);\n");
        print!("gl.compile_shader(&vsshader);\n");

        print!("let fscode = \"{}\";\n", fs);
        print!("let fsshader = gl.create_shader(t).jsok()?;\n",);
        print!("gl.shader_source(&fsshader, fscode);\n");
        print!("gl.compile_shader(&fsshader);\n");
    print!("}}");
    //let stage = ShaderStage::parse(vs);
    //assert!(stage.is_ok());

    //let mut counter = Counter { var_nb: 0 };
    //stage.expect("").visit(&mut counter);
}