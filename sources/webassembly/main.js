import 'babel-polyfill';
import mainUrl from './.bin/main.002.wasm';

async function importWASM (fileUrl, imp) {
    const wasm = fetch(fileUrl);
    const { module, instance } = 
        await WebAssembly.instantiateStreaming(wasm, imp);
    return instance.exports;
};

async function run () {
    const { add } = await importWASM(mainUrl, {
        env: {
            consoleLog: x => console.log(x)
        }
    });
    console.log(add(1,2));
};

run();