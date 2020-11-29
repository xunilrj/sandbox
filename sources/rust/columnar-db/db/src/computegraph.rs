use crate::computegraphdefinition::*;
use crate::datacatalog::*;
use crate::filemanager::*;
use crossbeam_channel::*;
use std::sync::*;
use std::vec::*;

struct ComputeTask {
    f: Box<dyn Fn(&Vec<Option<Receiver<Buffer>>>, &Vec<Option<Sender<Buffer>>>)>,
    // data: ComputeTaskData,
    inputs: Vec<Option<Receiver<Buffer>>>,
    outputs: Vec<Option<Sender<Buffer>>>,
}

impl ComputeTask {
    pub fn new<F>(f: F) -> Self
    where
        F: Fn(&Vec<Option<Receiver<Buffer>>>, &Vec<Option<Sender<Buffer>>>) + 'static,
    {
        Self {
            f: Box::new(f),
            inputs: Vec::new(),
            outputs: Vec::new(),
        }
    }

    pub fn start(&self) {
        let f = self.f.as_ref();
        f(&self.inputs, &self.outputs);
    }
}

pub struct ComputeGraph<'a> {
    tasks: Vec<ComputeTask>,

    catalog: &'a DataCatalog,
}

impl<'a> ComputeGraph<'a> {
    pub fn new(catalog: &'a DataCatalog) -> Self {
        Self {
            tasks: Vec::new(),
            catalog: catalog,
        }
    }

    pub fn new_node(&mut self, def: &ComputeTaskDefinitionTypes) {
        use ComputeTaskDefinitionFolds::*;
        use ComputeTaskDefinitionSources::*;
        use ComputeTaskDefinitionTypes::*;
        match def {
            Source(ReadColumn { column }) => {
                let column = column.to_string();
                let file = self.catalog.get_file(&column).to_string();

                let mut task = ComputeTask::new(move |i, o| {
                    let stdout = o.get(0).unwrap();
                    let stdout = stdout.as_ref().unwrap();
                    handle_read_all(&file, stdout);
                });
                task.outputs.push(None);
                self.tasks.push(task);
            }
            Fold(Maximum) => {
                let mut task = ComputeTask::new(move |i, o| {
                    let stdin = i.get(0).unwrap();
                    let stdin = stdin.as_ref().unwrap();

                    let mut r = std::u8::MIN;
                    let buffer = stdin.recv().unwrap();
                    for v in buffer.as_u8() {
                        r = r.max(*v);
                    }

                    println!("max {}", r);
                });
                task.inputs.push(None);
                self.tasks.push(task);
            }
        }
    }

    pub fn link(&mut self, sid: usize, spid: usize, did: usize, dpid: usize) {
        let (s, r) = unbounded::<Buffer>();
        let output = self
            .tasks
            .get_mut(sid)
            .unwrap()
            .outputs
            .get_mut(spid)
            .unwrap();
        *output = Some(s);

        let input = self
            .tasks
            .get_mut(did)
            .unwrap()
            .inputs
            .get_mut(dpid)
            .unwrap();
        *input = Some(r);
    }

    pub fn start(&mut self) {
        for t in &self.tasks {
            t.start();
        }
    }

    // pub fn wait(&self, timeout: std::time::Duration) {}

    // pub fn result(&self) -> Result<(), ()> {}
}
