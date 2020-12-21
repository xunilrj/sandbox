use goose::prelude::*;

async fn website_index(user: &GooseUser) -> GooseTaskResult {
    let _goose = user.get("/").await?;

    Ok(())
}

fn main() -> Result<(), GooseError> {
    GooseAttack::initialize()?
        .register_taskset(taskset!("LoadtestTasks").register_task(task!(website_index)))
        .execute()?
        .print();

    Ok(())
}
