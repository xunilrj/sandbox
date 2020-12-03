use columnar_db::computegraphdefinition::*;
use columnar_db::datacatalog::*;
use columnar_db::filemanager::*;
use columnar_db::threadpool::*;

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

fn main() {
    let now = std::time::Instant::now();
    println!("Creating 100M temp file");
    let file =
        TempFile::random("Table1.Column1.dat", 100 * 1024 * 1024).expect("Cannot create temo file");
    println!("Done");
    println!("{:?}", std::time::Instant::now() - now);

    let mut pool = Threadpool::new();
    pool.create_one_per_cpu().expect("Cannot create Threadpool");

    let _filemgr = FileManager::new(&mut pool);

    let mut catalog = DataCatalog::new();
    catalog.add("Table1.Column1", "Table1.Column1.dat");

    // let svcs = ServicesCatalog::new();
    // svcs.add(pool);
    // svcs.add(filemgr);
    // svcs.add(catalog);

    let mut def = ComputeGraphDefinition::new();
    let t1c1 = def.from_columns("Table1.Column1");
    let maxt1c1 = def.fold_source(&t1c1, ComputeTaskDefinitionFolds::Maximum);
    def.result_from_fold(&maxt1c1, "max from table1.column1");

    // let ctx = ExecutionContext::new(&svcs);
    let mut computation = def.build(&catalog);

    let now = std::time::Instant::now();
    computation.start(&mut pool);
    computation.wait(std::time::Duration::from_secs(1));

    println!(
        "Result: {:?}",
        computation.get_result("max from table1.column1")
    );
    println!("{:?}", std::time::Instant::now() - now);
    drop(file)
}
