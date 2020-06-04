var fs = require("fs");

function is_basic_data_type(type) {
    if (type.type == "_Bool") return true;
    if (type.type == "float") return true;
    return false;
}

function generateProxy(json) {
    if (!json) return "";
    var code = [];
    json.records.forEach(x => {
        var safeName = x.name.replace("::", "")
            .replace("struct ", "")
            .replace(">", "")
            .replace("<", "_")
            .replace(",", "_")
            .replace("  ", "")
            .replace(" ", "");
        // console.log(safeName);
        code.push(`export class ${safeName} {\n`);
        code.push(`constructor(dataView, memBase, offset) { this.__v = dataView; this.__b = memBase; this.__o = offset;}\n`);
        x.fields = x.fields || [];
        function initType(type, p) {
            var code = [];
            if (type.indexOf("struct") == 0) {
                var structName = type.split(" ")[1].replace("::", "")
                    .replace("struct ", "")
                    .replace(">", "")
                    .replace("<", "_")
                    .replace(",", "_")
                    .replace("  ", "")
                    .replace(" ", "");
                code.push(`new ${structName} (this.__v, this.__b, this.__o + ${p.offset}, true)`);
            } else if (type.indexOf("union") == 0) {
                var unionName = type.split(" ")[1].replace("::", "");
                code.push(`new ${unionName} (this.__v, this.__b, this.__o + ${p.offset}, true)`);
            }
            return code.join("");
        }
        x.fields.forEach(p => {
            code.push(`get ${p.name}() {`);
            var type = p.type.type;
            if (p.type.isArray && p.type.arraySize) {
                code.push(`var items = [];`);
                code.push(`for(var i=0;i<${p.type.arraySize};++i) {`);
                var newp = JSON.parse(JSON.stringify(p));
                newp.offset = `${p.offset} + (i * ${newp.type.size})`;
                code.push(`items.push(${initType(type, newp)})`);
                code.push(`}`);
                code.push(`return items;`);
            } else if (type == "int") {
                code.push(`return this.__v.getInt32(this.__o + ${p.offset}, true);`);
            } else if (type == "unsigned int") {
                code.push(`return this.__v.getUint32(this.__o + ${p.offset}, true);`);
            } else if (type == "float") {
                code.push(`return this.__v.getFloat32(this.__o + ${p.offset}, true);`);
            } else if (type == "_Bool") {
                code.push(`return this.__v.getUint8(this.__o + ${p.offset}, true) == 1;`);
            } else if (type == "const char *") {
                code.push("var str = '';var max = 1024;");
                code.push(`var adr = this.__b + this.__v.getUint32(this.o + ${p.offset}, true);`);
                code.push(`var c = this.__v.getUint8(adr, true);`);
                code.push("do {");
                code.push("str += String.fromCharCode(c);");
                code.push(`++adr; --max; c = this.__v.getUint8(adr, true);`);
                code.push("} while(c != 0 && max > 0);");
                code.push("return str;");
            } else if (type.indexOf("struct") == 0) {
                code.push(`return ${initType(type, p)};`);
            } else if (type.indexOf("union") == 0) {
                code.push(`return ${initType(type, p)};`);
            }
            code.push(`}\n`);
        });
        code.push('toJS() { return { ');
        x.fields.forEach(x => {
            if (is_basic_data_type(x.type))
                code.push(`${x.name}: this.${x.name}, `);
            else {
                if (x.type.isArray)
                    code.push(`${x.name}: this.${x.name}.map(x => x.toJS()), `);
                else
                    code.push(`${x.name}: this.${x.name}.toJS(), `);
            }
        });
        code.push('}; }\n');
        code.push(`}\n`);
    });;
    code.push(`export async function load(promise, memory, imp) { const module = await WebAssembly.instantiateStreaming(promise, imp); return new Proxy(new DataView(memory.buffer), 0, module.instance.exports); }\n`);
    code.push("export class Proxy {\n");
    code.push(`constructor(dataView, memBase, instance) { this.__v = dataView; this.__b = memBase; this.__i = instance; }\n`);
    json.functions = json.functions || [];
    json.functions.forEach(x => {
        x.arguments = x.arguments || [];
        var args = x.arguments.map(xx => xx.name).join(", ");
        code.push(`${x.name} (`);
        code.push(args);
        code.push(") {");
        var cleanReturnType = x.returnType.type.replace("::", "")
            .replace("struct ", "")
            .replace(">", "")
            .replace("<", "_")
            .replace(",", "_")
            .replace("  ", "")
            .replace(" ", "");
        var isCustomType = true;
        if (cleanReturnType == "const char")
            isCustomType = false;
        if (cleanReturnType == "float")
            isCustomType = false;

        let at = "66560";

        //TODO read from memory
        if (!isCustomType) {
            code.push(`return this.__i.${x.name}(${args}); }\n`);
        }
        else if (isCustomType && x.returnType.isPointer && !x.returnType.isReference) {
            code.push(`var r = this.__i.${x.name}(${args});\n`);
            code.push(`return new ${cleanReturnType} (this.__v, this.__b, r); }\n`);
        } else if (isCustomType && !x.returnType.isPointer && x.returnType.isReference) {
            code.push(`var r = this.__i.${x.name}(${args});\n`);
            code.push(`return new ${cleanReturnType} (this.__v, this.__b, r); }\n`);
        } else if (isCustomType && !x.returnType.isPointer && !x.returnType.isReference) {
            code.push(`this.__i.${x.name}(${at}, ${args}); `);
            code.push(`return new ${cleanReturnType} (this.__v, this.__b, ${at}); }\n`);
        } else {
            code.push("TODO; }\n");
        }
    });
    code.push(`}\n`);
    return code.join("");
}

var txt = fs.readFileSync(process.argv[2]);
var json = JSON.parse(txt);
var out = generateProxy(json);

console.log(out);