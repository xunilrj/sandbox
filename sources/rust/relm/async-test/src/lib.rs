// // #[async_trait::async_trait]
// // trait ActionHandler {
// //     async fn handle(&self);
// // }

// // struct Actions {}

// // #[async_trait::async_trait]
// // impl ActionHandler for Actions {
// async fn handle() {
//     unsafe { consolelog(54) };
//     SetTimeoutFuture(0, 5000).await;
//     unsafe { consolelog(55) };
// }
// // }

// #[no_mangle]
// pub fn init() {
//     // let action = Box::leak(Box::new(Actions {}));
//     let handle = handle();

//     let e = Box::leak(Box::new(Executor::new()));
//     e.spawn(handle);
// }

#[allow(dead_code)]
fn a() {}
