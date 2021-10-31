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

async function traceNext(qty) {
    while (qty > 0) {
        qty -= 1;
        await print(`${await getCurrentInstructionString()}`);
        await step();
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
    const { sp } = await getThreadContext();
    await print("Stack");
    await print("-----------");
    for (let offset = 4; offset < 0x44; offset += 4) {
        await print(`ESP+0x${offset.toString(16)} = ${await read("u32", sp + offset)}`);
    }
    await print("-----------");
}

async function printStruct(addr, offsets) {
    await print(`Struct @ ${addr}`);
    await print("-----------");
    let s = {};
    for (const k in offsets) {
        s[k] = await read("u32", addr + offsets[k]);
    }
    print(s);
    await print("-----------");
    return s;
}

async function main() {
    await addBreakpoint("CreateFileA");
    await init("C:\\Program Files (x86)\\Telltale Games\\Tales of Monkey Island\\Launch of the Screaming Narwhal\\MonkeyIsland101.exe");
    const { api } = await goUntil(x => x.name == "CreateFileA" && x.args.lpFileName.endsWith("sk20_guybrush.skl"));
    await addBreakpoint("ReadFile");

    await print(api.args.lpFileName);
    let i = 0;
    while (true) {
        await go();
        const api = await currentStackFrame();
        if (api.name == "ReadFile") {
            await goUntilReturn();
            const { ip, bp, sp } = await getThreadContext();
            const buffer = await readArray("u8", api.args.nNumberOfBytesToRead, api.args.lpBuffer);

            if (api.args.nNumberOfBytesToRead < 32) {
                print(`${i} 0x${ip.toString(16)} ${api.args.nNumberOfBytesToRead} [${buffer}] - ${best_type(buffer)} `);
            } else {
                print(i, "------------------------------------------")
                print(i, api.args.nNumberOfBytesToRead);
                print(i, "------------------------------------------")
            }

            i += 1;
        }

        // if (api.args.nNumberOfBytesToRead == 38896) {
        //     await goUntilReturn();
        //     await stepUntil((api, ctx) => ctx.ip == 0x0070FC81);

        //     const ctx = await getThreadContext();
        //     await print(ctx)
        //     await printStack();
        //     const b = await printStruct(await read("u32", ctx.sp + 0x24), {
        //         qty: 0x1c,
        //         size: 0x20,
        //         outputBuffer: 0x24,
        //     });

        //     await addBreakpoint(0x0070FEA1, { once: true });
        //     await goUntil((api, ctx) => ctx.ip == 0x0070FEA1);

        //     await print(await getThreadContext())
        //     await printStack();

        //     const ib = await readArray("u8", b.qty * 2, b.outputBuffer);
        //     await writeFile("C:\\Users\\xunil\\Downloads\\ttarctext\\mi101\\obj_ballcannon-real.ib", ib);
        // }

        if (api.name == "CreateFileA") {
            break;
        }
    }
}
main();