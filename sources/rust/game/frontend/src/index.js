async function init() {
    let x = await import("./game/wasm.js");
    x.init(0);
    x.greet();
}
init();