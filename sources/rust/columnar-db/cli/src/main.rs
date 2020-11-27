mod threadpool;
// use threadpool::*;

// struct DataCatalog
// {
// }

// impl DataCatalog
// {
//     pub fn new() -> Self
//     {

//     }

//     pub fn add(&mut self, column: &str, path: &str)
//     {

//     }
// }

// struct ExecutionContext
// {
// }

// impl ExecutionContext
// {
//     pub fn new(pool: &ServicesCatalog) -> Self
//     {

//     }

//     pub fn channel<T>() -> (WriteChannel<T>, ReadChannel<T>)
//     {
//     }
// }

// struct WriteChannel<T>
// {
//     output: Sender<T>,
// }

// struct ReadChannel<T>
// {
//     input: Receiver<T>,
// }

// struct Array
// {
// }

// enum ComputeTaskData
// {
//     ReadColumn { output: WriteChannel<Array> },
//     Maximum { input: ReadChannel<Array>, output: WriteChannel<Array> },
// }

// struct ComputeTask
// {
//     data: ComputeTaskData,
// }

// impl ComputeTask
// {
// }

// struct ComputeGraph
// {
//     tasks: Vec<ComputeTask>,
//     root: usize,
// }

// impl ComputeGraph
// {
//     pub fn start(&mut self)
//     {
//     }

//     pub fn wait(&self, timeout: std::time::Duration)
//     {

//     }

//     pub fn result(&self) -> Result<(),()>
//     {

//     }
// }

// enum ComputeTaskDefinitionSources
// {
//     ReadColumn { column: String }
// }

// enum ComputeTaskDefinitionFolders
// {
//     Maximum
// }

// struct ComputeTaskDefinition
// {
// }

// impl ComputeTaskDefinition
// {
//     pub fn fold(&mut self, next: ComputeTaskDefinitionFolders) -> &mut ComputeTaskDefinition
//     {
//     }
// }

// struct ComputeGraphDefinition
// {
// }

// impl ComputeGraphDefinition
// {
//     pub fn new() -> Self
//     {

//     }

//     pub fn source(&mut self, source: ComputeTaskDefinitionSources) -> &mut ComputeTaskDefinition
//     {

//     }

//     pub fn build(&self, ctx: &ExecutionContext) -> ComputeGraph
//     {

//     }
// }

// struct ServicesCatalog
// {
// }

// impl ServicesCatalog
// {
//     pub fn new() -> Self
//     {
//     }

//     pub fn add<T>(&mut self, svc: T)
//     {
//     }
// }

// struct FileManager
// {
// }

// impl FileManager
// {
//     pub fn new() -> Self
//     {
//     }
// }

fn main() {
    // let pool = Threadpool::new();
    // pool.create_one_per_cpu();

    // let filemgr = FileManager::new();

    // let catalog = DataCatalog::new();
    // catalog.add("Table1.Column1", "/data/Table1.Column1.dat");

    // let svcs = ServicesCatalog::new();
    // svcs.add(pool);
    // svcs.add(filemgr);
    // svcs.add(catalog);

    // let definition = ComputeGraphDefinition::new();
    // let a = definition.source(ComputeTaskDefinitionSources::ReadColumn {
    //     column: "Table1.Column1".to_string(),
    // });
    // let b = a.fold(ComputeTaskDefinitionFolders::Maximum);

    // let ctx = ExecutionContext::new(&svcs);
    // let mut computation = definition.build(&ctx);
    // computation.start();
    // computation.wait(std::time::Duration::from_secs(1));

    // println!("Result: {:?}", computation.result());
}
