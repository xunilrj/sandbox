async function goUntil(f) {
    while (true) {
        await go();
        const api = await currentStackFrame();
        const ctx = await getThreadContext();
        if (f && f.then && (await f(api, ctx))) {
            return { api, ctx };
        } else if (f && f(api, ctx)) {
            return { api, ctx };
        }
    }
}

async function stepUntil(f) {
    while (true) {
        await step();
        const api = await currentStackFrame();
        const ctx = await getThreadContext();
        if (f && f.then && (await f(api, ctx))) {
            return { api, ctx };
        } else if (f(api, ctx)) {
            return { api, ctx };
        }
    }
}

async function traceUntil(f) {
    while (true) {
        const api = await currentStackFrame();
        const ctx = await getThreadContext();
        if (f && f.then && (await f(api, ctx))) {
            return { api, ctx };
        } else if (f(api, ctx)) {
            return { api, ctx };
        }
        await printCurrentInstruction();
        await step();
    }
}

async function traceNext(qty, anm) {
    let ip;
    let i = 0;
    let tab = 0;
    let tabdelta = 0;
    let objects = {

    };
    let bufferMem = null;
    while (qty > 0) {
        qty -= 1;
        tab = Math.max(0, tab + tabdelta);
        tabdelta = 0;

        let ctx = await getThreadContext();
        ip = ctx.ip;
        // await IncrIP(ip);

        let print_instruction = true;
        const str = await getCurrentInstructionString();

      
        // if (bufferMem) {
        //     print_instruction = str.includes(bufferMem);
        // }

        if (str.includes("call")) {
            print_instruction = true;
            tabdelta = 1;
        }
          
        if (str.includes("ret")) {
            tabdelta = -1;
        }

        const ignore = [0x7354e0, 0x445ff0, 0x593e50, 0x6ebe60, 0x43d4f0, 0x41a5e0, 0x712b40, 0x496e70, 0x43dd00];  
        if (ignore.indexOf(ip) >= 0) {
            await goUntilReturn();
            tabdelta = -1;
        }

        // if ((ip >= 0x4972f0) && (ip <= 0x00497e50)) {
        //     print_instruction = true;
        // }

        // if (tab <= 5) {
        //     print_instruction = true;
        // }

        // //this one read the file
        // if ((ip >= 0x44a3a0) && (ip <= 0x0044a4c0)) {
        //     print_instruction = false;
        // }

        // if ((ip >= 0x738460) && (ip <= 0x738503)) {
        //     print_instruction = false;
        // }

        // this allocates memory. global new?
        // if ((ip >= 0x7630ca) && (ip <= 0x763176)) {
        //     print_instruction = false;
        // }

        if (print_instruction) {
            await print(`${str}`);
        }

        while (ip >= 0x70000000) {
            const api = await getFunctionAt(ip);
            await print(`${JSON.stringify(api)}`);
            await goUntilReturn();
            tabdelta = -1;
            if (api && api.name == "ReadFile") {
                const buffer = await readArray("u8", api.args.nNumberOfBytesToRead, api.args.lpBuffer);
                if (api.args.nNumberOfBytesToRead < 256) {
                    await print(`${i} ${api.args.lpBuffer} 0x${api.args.lpBuffer.toString(16)} ${api.args.nNumberOfBytesToRead} [${buffer}]`);
                } else {
                    await print(`${i} ${api.args.lpBuffer} 0x${api.args.lpBuffer.toString(16)} ${api.args.nNumberOfBytesToRead} [too big]`);
                }

                let length = 8;
                if (api.args.nNumberOfBytesToRead == length) {
                    bufferMem = `mem[${api.args.lpBuffer.toString()}`;
                }

                i += 1;
            }

            // if (api && api.name == "CreateFileA" && !api.args.lpFileName.includes(anm)) {
            //     return;
            // }
            
            ctx = await getThreadContext();
            ip = ctx.ip;
        }

        await step();

        // if (ip == 0x74a6c0) {
        //     objects["0x74a6c0"] = ctx.cx;
        // }

        // if ((ip >= 0x74a6c0) && (ip <= 0x74a986)) {
        //     await followStruct(tab, objects["0x74a6c0"], {
        //         "0x00": { delta: 0x00, type: "u32" },
        //         "0x04": { delta: 0x04, type: "u32" },
        //         "0x08": { delta: 0x08, type: "u32" },
        //         "0x0C": { delta: 0x0C, type: "u32" },
        //         "0x10": { delta: 0x10, type: "f32" },
        //         "0x14": { delta: 0x14, type: "f32" },
        //         "0x18": { delta: 0x18, type: "f32" },
        //         "0x1c": { delta: 0x1c, type: "u32" },
        //         "0x20": { delta: 0x20, type: "f32" },
        //         "0x24": { delta: 0x24, type: "u8" },
        //         "0x25": { delta: 0x25, type: "u8" },
        //         "0x26": { delta: 0x26, type: "u8" },
        //     });
        // }

        // if (ip == 0x6C6450) {
        //     let current = await getStruct(ctx.si, {
        //         "0x00": { delta: 0x00, type: "u32" },
        //         "0x04": { delta: 0x04, type: "u32" },
        //         "0x08": { delta: 0x08, type: "u32" },
        //         "0x0C": { delta: 0x0C, type: "u32" },
        //         "0x10": { delta: 0x10, type: "u32" },
        //         "0x14": { delta: 0x14, type: "u32" },
        //         "secondBuffer": { delta: 0x18, type: "u32" },
        //         "0x1C": { delta: 0x1C, type: "u32" },
        //         "0x20": { delta: 0x20, type: "u32" },
        //         "0x24": { delta: 0x24, type: "u32" },
        //         "0x28": { delta: 0x28, type: "u32" },
        //         "0x2C": { delta: 0x2C, type: "u32" },
        //         "0x30": { delta: 0x30, type: "u32" },
        //         "0x34": { delta: 0x34, type: "u32" },
        //         "0x38": { delta: 0x38, type: "u32" },
        //         "0x3C": { delta: 0x3C, type: "u32" },
                
        //         "firstBuffer": { delta: 0x40, type: "u32" },
        //         "0x44": { delta: 0x44, type: "u32" },
        //         "0x4C": { delta: 0x4C, type: "u32" },
        //         "0x50": { delta: 0x50, type: "u32" },

        //         "0x54": { delta: 0x54, type: "f32" },
        //         "0x58": { delta: 0x58, type: "f32" },
        //         "0x5c": { delta: 0x5c, type: "f32" },
        //         "0x60": { delta: 0x60, type: "f32" },
        //         "0x64": { delta: 0x64, type: "f32" },
        //         "0x68": { delta: 0x68, type: "f32" },
        //         "0x6c": { delta: 0x6c, type: "f32" },

        //         "0x70": { delta: 0x70, type: "f32" },
        //         "0x74": { delta: 0x74, type: "f32" },
        //         "0x78": { delta: 0x78, type: "f32" },
        //         "0x7c": { delta: 0x7c, type: "f32" },
        //         "0x80": { delta: 0x80, type: "f32" },
        //         "0x84": { delta: 0x84, type: "f32" },
        //         "0x88": { delta: 0x88, type: "f32" },

        //         "0x8c": { delta: 0x8c, type: "f32" },
        //         "0x90": { delta: 0x90, type: "f32" },
        //         "0x94": { delta: 0x94, type: "f32" },
        //         "0x98": { delta: 0x98, type: "f32" },
        //         "0x9c": { delta: 0x9c, type: "f32" },
        //         "0xa0": { delta: 0xa0, type: "f32" },
        //         "0xa4": { delta: 0xa4, type: "f32" },

        //         "0xa8": { delta: 0xa8, type: "f32" },
        //         "0xac": { delta: 0xac, type: "f32" },
        //         "0xb0": { delta: 0xb0, type: "f32" },
        //         "0xb4": { delta: 0xb4, type: "f32" },
        //         "0xb8": { delta: 0xb8, type: "f32" },
        //         "0xbc": { delta: 0xbc, type: "f32" },
        //         "0xc0": { delta: 0xc0, type: "f32" },

        //         "0xc4": { delta: 0xc4, type: "f32" },
        //         "0xc8": { delta: 0xc8, type: "f32" },
        //         "0xcc": { delta: 0xcc, type: "f32" },
        //         "0xd0": { delta: 0xd0, type: "f32" },
        //         "0xd4": { delta: 0xd4, type: "f32" },
        //         "0xd8": { delta: 0xd8, type: "f32" },
        //         "0xdc": { delta: 0xdc, type: "f32" },

        //         "0xe0": { delta: 0xe0, type: "u32" },
        //         "0xe4": { delta: 0xe4, type: "u32" },
        //         "0xe8": { delta: 0xe8, type: "u32" },
        //         "0xec": { delta: 0xec, type: "u32" },

        //         "0xf0": { delta: 0xf0, type: "u16" },
        //         "0xf2": { delta: 0xf2, type: "u8" },
        //         "0xf3": { delta: 0xf3, type: "u8" },
        //         "0xf4": { delta: 0xf4, type: "u8" },
        //         "0xf5": { delta: 0xf5, type: "u8" },
        //     });
        //     await print(`${JSON.stringify(current, null, 4)}`);
        // }
    }
}

async function goUntilReturn() {
    const { sp } = await getThreadContext();
    const addr = await read("u32", sp);
    await addBreakpoint(addr, { once: true });
    await goUntil((x, ctx) => ctx.ip == addr);
}

function best_type(arr) {
    const a = new Uint8Array(arr);
    if (arr.length == 4) {
        const integer = new Uint32Array(a.buffer)[0];
        return `u32[${integer}] (0x${integer.toString(16)}) or f32[${new Float32Array(a.buffer)[0]}]`;
    } else if (arr.length == 8) {
        const integer = new BigUint64Array(a.buffer)[0];
        return `u64[${integer}] (0x${integer.toString(16)})`;
    } else {
        return ``;
    }
}

async function printStack() {
    const { ip, bp, sp } = await getThreadContext();
    await print(`\tip=0x${ip.toString(16)}`);
    await print(`\tsp=0x${sp.toString(16)}`);
    await print(`\tbp=0x${bp.toString(16)}`);
    await print("\tStack");
    await print("\t-----------");
    for (let offset = 0; offset < 0x200; offset += 4) {
        let v = await read("u32", sp + offset);
        await print(`\t0x${(sp + offset).toString(16)} ESP+0x${offset.toString(16)} = ${v} (0x${v.toString(16)})`);
    }
    await print("\t-----------");
}

async function getStruct(addr, offsets) {
    let s = {_addr: addr};
    for (const k in offsets) {
        const { delta, type } = offsets[k];
        s[k] = await read(type, addr + delta);
    }
    return s;
}

async function printStruct(addr, offsets) {
    await print(`\tStruct @ ${addr}`);
    await print("\t-----------");
    let s = {};
    for (const k in offsets) {
        const { delta, type } = offsets[k];
        s[k] = await read(type, addr + delta);
    }
    await print(`\t${JSON.stringify(s, null, 4)}`);
    await print("\t-----------");
    return s;
}

let followStructs = {};
async function followStruct(tab, addr, s) {
    if (!followStructs[addr]) {
        followStructs[addr] = {};
    }
    
    let old = followStructs[addr];
    let current = await getStruct(addr, s);
    let [anydiff, d] = diff(old, current);
    if (anydiff) {
        await print(`${"\t".repeat(tab)}${JSON.stringify(d)}`);
    }

    followStructs[addr] = current;
    return 
}

function diff(a, b) {
    a = a || {};
    b = b || {};
    let r = {};
    let anydiff = false;
    for (const k in b) {
        if (a[k] == b[k]) {
            r[k] = a[k];
        } else {
            anydiff = true;
            r[k] = {
                from: a[k],
                to: b[k]
            }
        }
    }
    return [anydiff, r];
}

const IPS = {};
async function IncrIP(ip) {
    ip = await getFunctionAt(ip);
    ip = ip.toString(16);
    if (IPS[ip]) {
        IPS[ip] += 1;
    } else {
        IPS[ip] = 1;
    }
}

async function printAllCallsTo(fs) {
    for (const f of fs) {
        await addBreakpoint(f);
    }
    await init("C:\\Program Files (x86)\\Telltale Games\\Tales of Monkey Island\\Launch of the Screaming Narwhal\\MonkeyIsland101.exe");

    while(true) {
        const { api } = await goUntil(x => fs.indexOf(x.name) != -1);
        await print(api);
    }
}

async function printAllReadFile(anm) {
    await addBreakpoint("CreateFileA");
    await addBreakpoint("ReadFile");
    await init("C:\\Program Files (x86)\\Telltale Games\\Tales of Monkey Island\\Launch of the Screaming Narwhal\\MonkeyIsland101.exe");

    let i = 0;
    let api;
    while(true) {
        let r = await goUntil(x => x.name == "CreateFileA" && x.args.lpFileName.includes(anm));
        api = r.api;
        await print(r.api);
        while(true) {
            r = await goUntil(x => x.name == "ReadFile");
            api = r.api;
            await print(api);
            await goUntilReturn();
            
            const buffer = await readArray("u8", api.args.nNumberOfBytesToRead, api.args.lpBuffer);
            if (api.args.nNumberOfBytesToRead < 256) {
                await print(`${i} ${api.args.lpBuffer} 0x${api.args.lpBuffer.toString(16)} ${api.args.nNumberOfBytesToRead} [${buffer}]`);
            } else {
                await print(`${i} ${api.args.lpBuffer} 0x${api.args.lpBuffer.toString(16)} ${api.args.nNumberOfBytesToRead} [too big]`);
            }

            i += 1;
        }
    }
}

async function getStack(qty) {
    let ctx = await getThreadContext();
    let i = 0;
    let stack = {};
    for (let i = 0;i < qty; ++i) {
        const d = i + 1;
        stack[`arg_${i}`] = await read("u32", ctx.sp + (d*4));
    }
    return stack;
}

async function print_who_called_me() {
    let ctx = await getThreadContext();
    let return_to = await read("u32", ctx.sp);
    await print(ctx);
    await print(return_to);
}

async function printAnm(anm) {
    await addBreakpoint("CreateFileA");
    await init("C:\\Program Files (x86)\\Telltale Games\\Tales of Monkey Island\\Launch of the Screaming Narwhal\\MonkeyIsland101.exe");
    const { api } = await goUntil(x => x.name == "CreateFileA" && x.args.lpFileName.includes(anm));
    await print(api);
    // await trace(10000000, [0x7354e0, 0x5947c0, 0x6ebe60, 0x43d4f0, 0x41a5e0, 0x445ff0, 0x712b40, 0x496e70, 0x43dd00]);
    // await trace(500000, [0x7354e0, 0x5947c0, 0x6ebe60, 0x593a60, 0x603a69]);
    // await go();
    let i = 0;
    const breakpoints = {
        ["ReadFile"]: async (api) => {
            await print("------------------------------------------------------------");
            await print(api);
            await goUntilReturn();
            const buffer = await readArray("u8", api.args.nNumberOfBytesToRead, api.args.lpBuffer);
            await print(`\t${i} ${api.args.lpBuffer} 0x${api.args.lpBuffer.toString(16)} ${api.args.nNumberOfBytesToRead} [${buffer}]`);

            if (api.args.nNumberOfBytesToRead == 4) {
                const b = new ArrayBuffer(4);
                const ui8 = new Uint8Array(b);
                ui8[0] = buffer[0];
                ui8[1] = buffer[1];
                ui8[2] = buffer[2];
                ui8[3] = buffer[3];
                await print(`\tas f32: ${new Float32Array(b)[0]}`)
                await print(`\tas u32: ${new Uint32Array(b)[0]}`)
            }

            i += 1;

            const str = await getCurrentInstructionString();
            await print(str);
            await trace(200, [])
        },
        // Read the 7 values inside buffer
        // [0x6c5f40]: async (api, ctx) => {
        //     // let return_to = await read("u32", ctx.sp);
        //     // await print(ctx);
        //     // await print(return_to);
        //     // await print("------------------------------------------------------------")
        //     // await trace(10000, [])
        // },
        // Calls the function that reads the 7 values
        // [0x6C6762]: async (api,ctx) => {
        //     await print("------------------------------------------------------------")
        //     await trace(10000, [])
        // },
        // [0x6c545f]: async (api, ctx) => {
        //     const str = await getCurrentInstructionString();
        //     await print("------------------------------------------------------------")
        //     await print(str);
        //     await trace(4, [])
        // },
        //0x6c65f0
        //0x4a33c0
        //0x5be530 
        // [0x5be530]: async () => {
        //     await print_who_called_me();
        // }
        // [0x5edf30]: async (api, ctx) => {
        //     const str = await getCurrentInstructionString();
        //     await print("------------------------------------------------------------")
        //     await print(str);
        //     await trace(1000, [])
        // }
    }

    // const vip = [
    //     , // - reads compressedkey buffer
    //     //0x6cb3a3,
    //     //0x6CB386, // - store float value inside each chunk of the compressed key buffer - see st0
    //      0x6C64C9, // copy samples as floats to struct
    // 0x6C5548 divides the q0 by its max value
    // ];

    for (const ip of Object.keys(breakpoints)) {
        let where = ip;
        try {
            let n = parseInt(ip.toString())
            if (n) {
                where = n;
            }
        } catch (e) {
        }
        await print(`Breakpoint at: ${ip.toString()}`);
        await addBreakpoint(where);
    }
    while(true) {
        const { api, ctx } = await goUntil(({name},{ip}) => !!breakpoints[name] || !!breakpoints[ip]);
        const f = breakpoints[api.name] || breakpoints[ctx.ip];
        if (f) {
            await f(api, ctx);
        }
    }
}

async function getUnknownStruct(addr, f) {
    let v = await getStruct(addr, {
        a: {delta: 0, type: "u32" },
        b: {delta: 4, type: "u32" },
        c: {delta: 8, type: "u32" },
        d: {delta: 0xC, type: "u32" },
        e: {delta: 0x10, type: "u32" },
        f: {delta: 0x14, type: "u32" },
        g: {delta: 0x18, type: "u32" },
        h: {delta: 0x1C, type: "u32" },
        i: {delta: 0x20, type: "u32" },
        j: {delta: 0x24, type: "u32" },
        k: {delta: 0x2C, type: "u32" },
        m: {delta: 0x30, type: "u32" },
        n: {delta: 0x34, type: "u32" },
        o: {delta: 0x38, type: "u32" },
        p: {delta: 0x3C, type: "u32" },
        q: {delta: 0x40, type: "u32" },
        r: {delta: 0x44, type: "u32" },
    });
    if (f) {
        await f(v);
    }
    await print(`\t${JSON.stringify(v)}`);
    return v
}

async function main() {
    //await printAllCallsTo(["CreateFileA", "FindFirstFileA"]);
    //await printAllReadFile("sk20_move_guybrushwalkdeterminedship.anm");
    
    // await printAnm("sk20_move_guybrushwalkdeterminedship.anm");
    await printAnm("sk20_guybrush.d3dmesh");


    // let anm = "sk20_move_guybrushwalkdeterminedship.anm";
    // await addBreakpoint("CreateFileA");
    // await init("C:\\Program Files (x86)\\Telltale Games\\Tales of Monkey Island\\Launch of the Screaming Narwhal\\MonkeyIsland101.exe");
    // const { api } = await goUntil(x => x.name == "CreateFileA" && x.args.lpFileName.includes(anm));



    // await addBreakpoint(0x434EA4);
    // while(true) {
    //     const {ctx} = await goUntil((x,{di}) => di == 535119246);
    //     await print(ctx);
    //     let v = await getStruct(ctx.ax, {
    //         header: {delta: 0, type: "u32" },
    //         a: {delta: 0x4, type: "u32" },
    //         hasha: {delta: 0x8, type: "u32" },
    //         hashb: {delta: 0xC, type: "u32" },
    //         flags: {delta: 0x10, type: "u32" },
    //         c: {delta: 0x14, type: "u32" },
    //         d: {delta: 0x18, type: "u32" },
    //         type: {delta: 0x1C, type: "u32" },
    //         f: {delta: 0x20, type: "u32" },
    //         next: {delta: 0x24, type: "u32" },
    //         bb: {delta: 0x28, type: "u32" },
    //         cc: {delta: 0x2C, type: "u32" },
    //         dd: {delta: 0x30, type: "u32" },
    //     });
    //     await print(`\t${JSON.stringify(v)}`);

    //     let vbb = await getUnknownStruct(v.bb, async x => {
    //         x.ctor = x.a;
    //         delete x.a;
    //     });

    //     //v.a - not a pointer
    //     //v.b - not a pointer
    //     //v.e
    //     let vtype = await getUnknownStruct(v.type, async x => {
    //         x.name = await read("nullString", x.a);
    //         x.flags = x.c;
    //         x.properties = x.g;
    //         delete x.a;
    //         delete x.c;
    //         delete x.g;
    //     });
    //     let vproperties = await getUnknownStruct(vtype.properties, async x => {
    //         x.hashb = x.c;
    //         x.hasha = x.d;
    //         x.flags = x.e;
    //         x.first = x.h;
    //         x.size = x.f;
    //         delete x.c;
    //         delete x.d;
    //         delete x.e;
    //         delete x.h;
    //         delete x.f;
    //     });
    //     let next = vproperties.first;
    //     let i = 10;
    //     while(true) {
    //         let vprop = await getUnknownStruct(next, async x => {
    //             x.name = await read("nullString", x.a);
    //             x.next = x.e;
    //             x.size = x.b;
    //             x.offset = x.i;
    //             x.properties = x.d;
    //             delete x.a;
    //             delete x.e;
    //             delete x.b;
    //             delete x.i;
    //             delete x.d;
    //         });
    //         next = vprop.next;
    //         i--;

    //         if (i <= 0) {
    //             break;
    //         }
    //         if (!next) {
    //             break;
    //         }
    //     }

    //     await trace(100);
    // }

}
main();