use columnar_db::computegraphdefinition::*;
use columnar_db::datacatalog::*;
use columnar_db::filemanager::*;
use columnar_db::threadpool::*;

fn main() {
    let now = std::time::Instant::now();
    println!("Creating 100M temp file");
    let file =
        TempFile::random("Table1.Column1.dat", 100 * 1024 * 1024).expect("Cannot create temp file");
    println!(
        "Done @ {:?} took {:?}",
        file.path,
        std::time::Instant::now() - now
    );

    let mut pool = Threadpool::new();
    pool.create_one_per_cpu().expect("Cannot create Threadpool");

    let _filemgr = FileManager::new(&mut pool);

    let mut catalog = DataCatalog::new();
    catalog.add("Table1.Column1", &file.path);

    let mut def = ComputeGraphDefinition::new();
    let t1c1 = def.from_columns("Table1.Column1");
    let maxt1c1 = def.fold_source(&t1c1, ComputeTaskDefinitionFolds::Maximum);
    def.result_from_fold(&maxt1c1, "max from table1.column1");
    println!("Compute Graph:");
    println!("{:?}", def);

    let mut computation = def.build(&catalog);

    let now = std::time::Instant::now();
    computation.start(&mut pool);
    computation.wait(std::time::Duration::from_secs(1));

    println!(
        "Result: {:?} took {:?}",
        computation.get_result("max from table1.column1"),
        std::time::Instant::now() - now
    );
}
