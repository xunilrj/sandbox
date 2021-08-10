use crate::channel::*;
use crate::computegraphdefinition::*;
use crate::datacatalog::*;
use crate::filemanager::*;
use crate::threadpool::*;
use crossbeam_channel::*;
use std::collections::*;
use std::sync::*;
use std::vec::*;

struct ComputeTaskData {
    f: Box<dyn Fn(&Vec<Option<Receiver<Message>>>, &Vec<Option<Sender<Message>>>) + Send>,
    // data: ComputeTaskData,
    inputs: Vec<Option<Receiver<Message>>>,
    outputs: Vec<Option<Sender<Message>>>,
}

impl std::fmt::Debug for ComputeTaskData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ComputeTaskData").finish_non_exhaustive()
    }
}

impl ComputeTaskData {
    pub fn new<F>(f: F) -> Self
    where
        F: Fn(&Vec<Option<Receiver<Message>>>, &Vec<Option<Sender<Message>>>) + Send + 'static,
    {
        Self {
            f: Box::new(f),
            inputs: Vec::new(),
            outputs: Vec::new(),
        }
    }
}

#[derive(Debug)]
struct ComputeTask {
    debug: String,
    data: Option<ComputeTaskData>,
}

impl ComputeTask {
    pub fn new<F>(debug: &str, f: F) -> Self
    where
        F: Fn(&Vec<Option<Receiver<Message>>>, &Vec<Option<Sender<Message>>>) + Send + 'static,
    {
        Self {
            debug: debug.to_string(),
            data: Some(ComputeTaskData::new(f)),
        }
    }

    fn reserve_channels(&mut self, qty_in: usize, qty_out: usize) {
        if let Some(data) = &mut self.data {
            for _ in 0..qty_in {
                data.inputs.push(None);
            }

            for _ in 0..qty_out {
                data.outputs.push(None);
            }
        }
    }

    pub fn start(&mut self, pool: &mut Threadpool) {
        let data = self.data.take().unwrap();
        let _ = pool.run("ComputeTask::start", move || {
            let f = data.f.as_ref();
            f(&data.inputs, &data.outputs);
        });
    }
}

#[derive(Debug)]
pub struct ComputeGraph<'a> {
    tasks: Vec<ComputeTask>,
    results: Arc<RwLock<HashMap<String, u8>>>,
    results_receivers: Vec<Receiver<()>>,

    senders: Vec<Sender<Message>>,
    receivers: Vec<Receiver<Message>>,

    catalog: &'a DataCatalog,
}

impl<'a> ComputeGraph<'a> {
    pub fn new(catalog: &'a DataCatalog) -> Self {
        Self {
            tasks: Vec::new(),
            results: Arc::new(RwLock::new(HashMap::new())),
            results_receivers: Vec::new(),
            catalog: catalog,
            senders: Vec::new(),
            receivers: Vec::new(),
        }
    }

    fn read_from_column_task(&mut self, column: &str) {
        let column = column.to_string();
        let file = self.catalog.get_file(&column).to_path_buf();

        let mut task = ComputeTask::new("from column", move |_, o| {
            let stdout = o.get(0).unwrap();
            let stdout = stdout.as_ref().unwrap();
            handle_read_all(&file, stdout);
        });
        task.reserve_channels(0, 1);
        self.tasks.push(task);
    }

    fn fold_maximum(&mut self) {
        let mut task = ComputeTask::new("fold", move |i, o| {
            let stdin = i.get(0).unwrap();
            let stdin = stdin.as_ref().unwrap();

            let mut max_value = std::u8::MIN;
            loop {
                match stdin.recv() {
                    Ok(Message::Buffer(buffer)) => {
                        for v in buffer.as_u8() {
                            max_value = max_value.max(*v);
                        }
                    }
                    Ok(Message::Eof) => {
                        let stdout = o.get(0).unwrap();
                        let stdout = stdout.as_ref().unwrap();

                        let mut buffer = Buffer::new();
                        buffer.push_u8(max_value);
                        let _ = stdout.send(Message::Buffer(buffer));
                        break;
                    }
                    _ => {}
                }
            }
        });
        task.reserve_channels(1, 1);
        self.tasks.push(task);
    }

    fn result_node(&mut self, name: &str) {
        let (s, r) = bounded(1);
        self.results_receivers.push(r);

        let name = name.to_string();
        let result_map = self.results.clone();
        let mut task = ComputeTask::new("result_node", move |i, _| {
            let stdin = i.get(0).unwrap();
            let stdin = stdin.as_ref().unwrap();
            match stdin.recv() {
                Ok(Message::Buffer(buffer)) => {
                    let &v = buffer.as_u8().get(0).unwrap();
                    let mut result_map = result_map.write().unwrap();
                    result_map.insert(name.clone(), v);
                    drop(result_map);
                    let _ = s.send(());
                }
                _ => {
                    todo!()
                }
            }
        });
        task.reserve_channels(1, 0);
        self.tasks.push(task);
    }

    pub fn new_node(&mut self, def: &ComputeTaskDefinitionTypes) {
        use ComputeTaskDefinitionFolds::*;
        use ComputeTaskDefinitionSources::*;
        use ComputeTaskDefinitionTypes::*;
        match def {
            Source(ReadColumn { column }) => self.read_from_column_task(column.as_str()),
            Fold(Maximum) => self.fold_maximum(),
            Result(name) => self.result_node(name),
        }
    }

    pub fn link(&mut self, sid: usize, spid: usize, did: usize, dpid: usize) {
        let (s, r) = unbounded::<Message>();
        let output = self.tasks.get_mut(sid).unwrap();
        let output = output.data.as_mut().unwrap();
        let output = output.outputs.get_mut(spid).unwrap();
        *output = Some(s.clone());

        let input = self.tasks.get_mut(did).unwrap();
        let input = input.data.as_mut().unwrap();
        let input = input.inputs.get_mut(dpid).unwrap();
        *input = Some(r.clone());

        self.senders.push(s);
        self.receivers.push(r);
    }

    pub fn start(&mut self, pool: &mut Threadpool) {
        for t in &mut self.tasks {
            t.start(pool);
        }
    }

    pub fn wait(&self, timeout: std::time::Duration) {
        let deadline = std::time::Instant::now() + timeout;

        for x in &self.results_receivers {
            let r = x.recv_deadline(deadline);
        }
    }

    pub fn get_result(&self, name: &str) -> Option<u8> {
        let r = self.results.read().unwrap();
        match r.get(name) {
            Some(&v) => Some(v),
            None => None,
        }
    }
}
