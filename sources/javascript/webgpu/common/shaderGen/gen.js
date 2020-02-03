const fs = require("fs");
const filePath = process.argv[2];

const variables = /^layout\s*\(\s*location\s*\=\s*(?<LOCATION>\d+)\s*\)\s*(?<DIRECTION>in|out)\s(?<TYPE>.*?)\s(?<NAME>.*?);$/gm;
const shader = fs.readFileSync(filePath);

var match;
var code = [];
var offset = 0;
do {
    match = variables.exec(shader);
    if (match && match.groups.DIRECTION == "in") {
        const location = match.groups.LOCATION;
        const name = match.groups.NAME;
        code.push(`const ${name}AttribDesc = { shaderLocation: ${location}, offset: ${offset}, format: 'float3' };\n`);
    }
} while (match);

code.push(`const positionBufferDesc = { attributes: [ positionAttribDesc ], arrayStride: 4 * 3 };\n`);
code.push(`const colorBufferDesc = { attributes: [ colorAttribDesc ], arrayStride: 4 * 3 };\n`);
code.push(`const vertexState = { indexFormat: 'uint16', vertexBuffers: [ positionBufferDesc, colorBufferDesc ] };\n`);
code.push(`const vertexStage = { module: vs, entryPoint: 'main' };\n`);
code.push(`const fragmentStage = { module: fs, entryPoint: 'main' };\n`);

console.log(code.join(""));