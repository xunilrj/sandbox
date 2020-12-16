import morphdom from './web_modules/morphdom.js';

let wasm = {
    obj: null
}
const decoder = new TextDecoder();
function consolef(category, size, ptr) {
    const { memory } = wasm.obj.instance.exports;
    const array = new Uint8Array(memory.buffer, Number(ptr), Number(size))
    const txt = decoder.decode(array);
    try {
        const obj = JSON.parse(txt);
        if (category == 0) console.error(obj);
        else if (category == 1) console.warn(obj);
        else if (category == 2) console.info(obj);
        else if (category == 3) console.debug(obj);
        else if (category == 4) console.trace(obj);
    }
    catch (e) {
        if (category == 0) console.error(txt);
        else if (category == 1) console.warn(txt);
        else if (category == 2) console.info(txt);
        else if (category == 3) console.debug(txt);
        else if (category == 4) console.trace(txt);
    }


}
function set_timeout(duration, handle) {
    setTimeout(() => {
        const { wake_by_id } = wasm.obj.instance.exports;
        wake_by_id(handle);
    }, Number(duration));
}
function apply_to(size, ptr) {
    const { memory } = wasm.obj.instance.exports;
    const array = new Uint8Array(memory.buffer, Number(ptr), Number(size))
    const html = decoder.decode(array);
    // console.log(html);

    var parser = new DOMParser();
    var doc = parser.parseFromString(html, 'text/html');

    morphdom(
        document.getElementById("root"),
        doc.body.firstChild
    )
}

export default function (id) {
    return WebAssembly.instantiateStreaming(fetch('content/app.wasm'), { env: { apply_to, console: consolef, set_timeout } })
        .then(obj => {
            wasm.obj = obj;

            window.send = function (appid, actorid, msgid, e) {

                const enc = new TextEncoder();

                e = e || [];

                let ps = [];
                for (let v of e) {
                    if (typeof (v) == 'string') {
                        let ptr = Number(obj.instance.exports.alloc(BigInt(v.length)));
                        ps.push(ptr);

                        const array = new Uint8Array(obj.instance.exports.memory.buffer, ptr, v.length);
                        enc.encodeInto(v, array);
                    }
                }
                let r = obj.instance.exports.application_send(appid, actorid, msgid,
                    BigInt(ps[0] || 0), BigInt(ps[1] || 0), BigInt(ps[2] || 0), BigInt(ps[3] || 0), BigInt(ps[4] || 0));
            }

            obj.instance.exports.init();
            const appid = obj.instance.exports.application_new();
            // const wrapperid = obj.instance.exports.application_mount(appid, 1);

            return (id) => {
                wasm.obj.instance.exports.application_mount(0, id);
            }
        });
};