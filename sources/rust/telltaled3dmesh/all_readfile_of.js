async function goUntil(f) {
    while (true) {
        await go();
        const api = await currentStackFrame();
        const ctx = await getThreadContext();
        if (f && f.then && (await f(api, ctx))) {
            return { api, ctx };
        } else if (f(api, ctx)) {
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

        let print_instruction = false;
        const str = await getCurrentInstructionString();

      
        if (bufferMem) {
            print_instruction = str.includes(bufferMem);
        }

        if (str.includes("call")) {
            print_instruction = true;
            tabdelta = 1;
        }
          
        if (str.includes("ret")) {
            tabdelta = -1;
        }

        const ignore = [0x7354e0, 0x5947c0, 0x6ebe60, 0x43d4f0, 0x41a5e0, 0x445ff0, 0x712b40, 0x496e70, 0x43dd00];  
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

                if (api.args.nNumberOfBytesToRead == 195) {
                    await print(`From now on, only if uses ${ api.args.lpBuffer}`)
                    while(true) {
                        await step();
                        await goUntilUsesMem(api.args.lpBuffer);
                    }
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

async function printAllCreateFileA() {
    await addBreakpoint("CreateFileA");
    await init("C:\\Program Files (x86)\\Telltale Games\\Tales of Monkey Island\\Launch of the Screaming Narwhal\\MonkeyIsland101.exe");

    while(true) {
        const { api } = await goUntil(x => x.name == "CreateFileA");
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

async function printAnm(anm) {
    await addBreakpoint("CreateFileA");
    await init("C:\\Program Files (x86)\\Telltale Games\\Tales of Monkey Island\\Launch of the Screaming Narwhal\\MonkeyIsland101.exe");
    const { api } = await goUntil(x => x.name == "CreateFileA" && x.args.lpFileName.includes(anm));
    await addBreakpoint("ReadFile");
    
    // await addBreakpoint(0x6c4450);
    // await addBreakpoint(0x6c6460);
    // await addBreakpoint(0x6c65f0);
    // await addBreakpoint(0x6c7050);
    // await addBreakpoint(0x6c70c0);
    // await addBreakpoint(0x6c7160);

    await print(api);
    await traceNext(10000000, anm);

    while (true) {
         await go();
         await print(`\t${await getCurrentInstructionString()}`);
    }
}

async function main() {
    // await printAllCreateFileA();
    //await printAllReadFile("sk20_move_guybrushwalkdeterminedship.anm");
    await printAnm("sk20_move_guybrushwalkdeterminedship.anm");
    
    // await addBreakpoint(0x74a6c0);
    // await addBreakpoint(0x74aa80);
    // while (true) {
    //      await go();
    //      const { ip, bp, sp, cx, si } = await getThreadContext();
    //      await print(`\t${await getCurrentInstructionString()}`);
    // }

    // let i = 0;
    // while (true) {
    //     await go();
    //     const api = await currentStackFrame();
    //     const { ip, bp, sp, cx, si } = await getThreadContext();

    //     if (i == 963) {
    //         break
    //     }

    //     // if (ip == 0x74aa80) {
    //     //     const { cx } = await getThreadContext();
    //     //     const addr = cx;
    //     //     let last_s;
    //     //     let last_json;
    //     //     await print(`Start @ ${addr} (0x${addr.toString(16)})`);
    //     //     while (true) {
    //     //         const { ip, bp, sp, cx, si } = await getThreadContext();
    //     //         if (ip == 0x0074ab96) {
    //     //             break;
    //     //         }
    //     //         await print(`\t ${await getCurrentInstructionString()}`);
    //     //         await step();
    //     //         const current_s = await getS(addr);
    //     //         const current_json = JSON.stringify(current_s);
    //     //         if (current_json != last_json) {
    //     //             print(`\t${JSON.stringify(diff(last_s, current_s), null, 4)}`);
    //     //         }
    //     //         last_s = current_s;
    //     //         last_json = current_json;
    //     //     }
    //     //     continue;
    //     // }

    //     if (api.name == "ReadFile") {
    //         await goUntilReturn();

    //         const buffer = await readArray("u8", api.args.nNumberOfBytesToRead, api.args.lpBuffer);

    //         if (api.args.nNumberOfBytesToRead < 256) {
    //             print(`${i} ${api.args.lpBuffer} 0x${api.args.lpBuffer.toString(16)} ${api.args.nNumberOfBytesToRead} [${buffer}]`);
    //         } else {
    //             print(`${i} ${api.args.lpBuffer} 0x${api.args.lpBuffer.toString(16)} ${api.args.nNumberOfBytesToRead} [too big]`);
    //         }

    //         await print(`\t${JSON.stringify(api)} `);
    //         // await printStack();
    //         try {
    //             await traceNext(100);
    //         }
    //         catch (e) {
    //             break
    //         }

    //         i += 1;
    //         continue
    //     }

    //     if (api.name == "CreateFileA") {

    //         break;
    //     }
    // }

    // await print(IPS);

}
main();