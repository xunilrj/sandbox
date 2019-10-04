import spine from './spine.json';
import {div, span, button} from 'hyperaxe';
import atlas1 from './atlas1.png';

function gen(i, f, j = "")
{
    var str = [];
    for(var ii = 0; ii < i; ++ii)
    str.push(f(ii));
    return str.join(j);
}

function createSkinnedShader(gl, n)
{
    let vsshader = gl.createShader(gl.VERTEX_SHADER);

    const vertexName = "a_pos_b";
    const vshader = `
//xy bind pos
//z weigth
${gen(n, (i) => `attribute vec3 ${vertexName}${i};\n`)}
attribute vec2 a_texcoords;

uniform mat3  u_world;
${gen(n, (i) => `uniform mat3  u_bone${i};\n`)}

varying highp vec2 out_texcoords;
void main() {
${gen(n, (i) => `\tvec3 pos${i} = ${vertexName}${i}.z * (vec3(${vertexName}${i}.xy, 1.0) * u_bone${i});\n`)}

    vec3 pos = ${gen(n, (i) => `pos${i}`, " + ")};
    pos = vec3(pos.xy, 1) * u_world;
    gl_Position = vec4(pos, 1.0);
    out_texcoords = a_texcoords;
}
`;
console.log(vshader);
    const fshader = ` precision mediump float;
uniform sampler2D u_texture0;
varying highp vec2 out_texcoords;

void main() {
    gl_FragColor = texture2D(u_texture0, out_texcoords);
}
`;

    gl.shaderSource(vsshader, vshader);
    gl.compileShader(vsshader);
    if (!gl.getShaderParameter(vsshader, gl.COMPILE_STATUS)) {
        console.log(gl.getShaderInfoLog(vsshader));
    }
    var fsshader = gl.createShader(gl.FRAGMENT_SHADER);
    gl.shaderSource(fsshader, fshader);
    gl.compileShader(fsshader);
    if (!gl.getShaderParameter(fsshader, gl.COMPILE_STATUS)) {
        console.log(gl.getShaderInfoLog(fsshader));
    }

    var shaderProgram = gl.createProgram();
    gl.attachShader(shaderProgram, vsshader);
    gl.attachShader(shaderProgram, fsshader);
    gl.linkProgram(shaderProgram);

    var shader = {
        a_pos_b: [],
        u_bone: [],
        a_texcoords: gl.getAttribLocation(shaderProgram, "a_texcoords"),        
        u_texture0: gl.getUniformLocation(shaderProgram, 'u_texture0'),
        u_world: gl.getUniformLocation(shaderProgram, "u_world")
    };

    for(var i = 0; i < n; ++i)
    {
        shader.a_pos_b.push(
            gl.getAttribLocation(shaderProgram, `a_pos_b${i}`)
        );
        shader.u_bone.push(
            gl.getUniformLocation(shaderProgram, `u_bone${i}`)
        );
    }

    shader.fill = (bones, tex2f, indices) => {
        var instance = {
            n: n,
            positionBuffers: [],
            texCoordBuffer: gl.createBuffer(),
            indexBuffer: gl.createBuffer(),
            qtd: indices.length, 
        };

        for(var i = 0;i < tex2f.length; i+=2)
        {
            var [u,v] = adjustUV(tex2f[i+0], tex2f[i+1]);
            tex2f[i+0] = u;
            tex2f[i+1] = v;
        }

        var vbuffer = [];

        for(var i = 0; i < n; ++i)
        {
            instance.positionBuffers.push(
                gl.createBuffer()
            );
            vbuffer.push([]);
        }
        
        //adjust vertex buffer
        var bones = [];
        var bonei = {};

        for(var i = 0; i < head.vertices.length; ++i)
        {
            var count = head.vertices[i]; ++i;
            for(var j = 0; j < count; ++j)
            {
                var bi = head.vertices[i]; ++i;
                if(!bonei[bi])
                {
                    bones.push(bi)
                    bi = bonei[bi] = bones.length - 1;
                }

                ++i; ++i; ++i;
            }
            --i;
        }

        if(bones.length > n) console.error("too many bones", bones);
        instance.bones = bones;    
        
        for(var i = 0; i < head.vertices.length; ++i)
        {
            var count = head.vertices[i]; ++i;
            var j = 0;

            var assignedBones = Array.from({length: n}, (el, index) => index);
            for(j = 0; j < count; ++j)
            {
                var bi = bonei[head.vertices[i]]; ++i;
                if(!bi) console.error("bone index");
                
                var index = assignedBones.indexOf(bi);
                if (index !== -1) assignedBones.splice(index, 1);

                var bbx = head.vertices[i]; ++i;
                var bby = head.vertices[i]; ++i;
                var w = head.vertices[i]; ++i;

                var buffer = vbuffer[bi];
                buffer.push(bbx);
                buffer.push(bby);
                buffer.push(w);
            }

            assignedBones.forEach(j => {
                var buffer = vbuffer[j];
                buffer.push(0);
                buffer.push(0);
                buffer.push(0);
            });
            --i;
        }

        //buffer data

        for(var i = 0; i < n; ++i)
        {
            //console.log(vbuffer[i]);
            gl.bindBuffer(gl.ARRAY_BUFFER, instance.positionBuffers[i]);
            gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(vbuffer[i]), gl.STATIC_DRAW);
            var error = gl.getError();
        }

        gl.bindBuffer(gl.ARRAY_BUFFER, instance.texCoordBuffer);
        gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(tex2f), gl.STATIC_DRAW);
        error = gl.getError();

        gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, instance.indexBuffer);
        gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, new Uint16Array(indices), gl.STATIC_DRAW);
        error = gl.getError();

        return instance;
    }

    shader.render = (instance, bones) => {
        var {positionBuffers, 
            texCoordBuffer, 
            indexBuffer,
            qtd} = instance;

        gl.useProgram(shaderProgram);

        gl.activeTexture(gl.TEXTURE0);
        gl.bindTexture(gl.TEXTURE_2D, instance.texture0);
        gl.uniform1i(shader.u_texture0, instance.texture0);

        for(var i = 0; i < n; ++i)
        {
            gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffers[i]);
            gl.vertexAttribPointer(shader.a_pos_b[i], 3, gl.FLOAT, false, 0, 0);
            gl.enableVertexAttribArray(shader.a_pos_b[i]);
        }

        gl.bindBuffer(gl.ARRAY_BUFFER, texCoordBuffer);
        gl.vertexAttribPointer(shader.a_texcoords, 2, gl.FLOAT, false, 0, 0);
        gl.enableVertexAttribArray(shader.a_texcoords);

        var pmatrix33f = gl.getFloatv(gl.PROJECTION);            
        gl.uniformMatrix3fv(shader.u_world, false, pmatrix33f);

        instance.bones.forEach((b,i) => {
            gl.uniformMatrix3fv(shader.u_bone[i], false, bones[b].matrix);
        });

        gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indexBuffer);
        gl.drawElements(gl.TRIANGLES, qtd, gl.UNSIGNED_SHORT, 0);
    }

    return shader;
}

function immediateMode2D(gl)
{
    let vertex2f = [];
    let positionBuffer;
    let tex2f = [];
    let texCoordBuffer;
    let indices = [];
    let indexBuffer;

    let vshader = document.getElementById("vshader").innerText;
    let fshader = document.getElementById("fshader").innerText;
    let vsshader = gl.createShader(gl.VERTEX_SHADER);

    let shaderProgram;

    let a_coords;
    let a_texcoords;
    let u_texture0;

    gl.noTexture = null;

    let mode;
    let matrixmode;

    let u_matrix;
    let pmatrix33f = [1, 0, 0, 0, 1, 0, 0, 0, 1];
    let matrix33f = [1, 0, 0, 0, 1, 0, 0, 0, 1];

    /////////////////////////////////////////////// SHADERS
    gl.shaderSource(vsshader, vshader);
    gl.compileShader(vsshader);
    if (!gl.getShaderParameter(vsshader, gl.COMPILE_STATUS)) {
        console.log(gl.getShaderInfoLog(vsshader));
    }
    var fsshader = gl.createShader(gl.FRAGMENT_SHADER);
    gl.shaderSource(fsshader, fshader);
    gl.compileShader(fsshader);
    if (!gl.getShaderParameter(fsshader, gl.COMPILE_STATUS)) {
        console.log(gl.getShaderInfoLog(fsshader));
    }

    ////////////////////////////////////////////// SHADER

    shaderProgram = gl.createProgram();
    gl.attachShader(shaderProgram, vsshader);
    gl.attachShader(shaderProgram, fsshader);
    gl.linkProgram(shaderProgram);

    a_coords = gl.getAttribLocation(shaderProgram, 'a_coords');
    a_texcoords = gl.getAttribLocation(shaderProgram, 'a_texcoords');
    u_texture0 = gl.getUniformLocation(shaderProgram, 'u_texture0');

    u_matrix = gl.getUniformLocation(shaderProgram, "u_matrix");

    ////////////////////////////////////////////// TEXTURE
    var level = 0;
    var internalFormat = gl.RGBA;
    var width = 100;
    var height = 100;
    var border = 0;
    var srcFormat = gl.RGBA;
    var srcType = gl.UNSIGNED_BYTE;
    gl.noTexture = gl.createTexture();
    gl.bindTexture(gl.TEXTURE_2D, gl.noTexture);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
    var pixels = new Uint8Array(width*height*4);            
    pixels.fill(255);
    gl.texImage2D(gl.TEXTURE_2D, level, internalFormat,
            width, height, border, srcFormat, srcType,
            pixels);

    gl.newTexture = (url) => {
        const texture = gl.createTexture();
        
        gl.bindTexture(gl.TEXTURE_2D, texture);
        const level = 0;
        const internalFormat = gl.RGBA;
        const width = 1;
        const height = 1;
        const border = 0;
        const srcFormat = gl.RGBA;
        const srcType = gl.UNSIGNED_BYTE;
        const pixel = new Uint8Array([0, 0, 255, 255]);  // opaque blue
        gl.texImage2D(gl.TEXTURE_2D, level, internalFormat,
                        width, height, border, srcFormat, srcType,
                        pixel);

        let r = {texture: texture, ready: false};

        const image = new Image();
        image.onload = function() {
            gl.bindTexture(gl.TEXTURE_2D, texture);
            gl.texImage2D(gl.TEXTURE_2D, level, internalFormat,
                        srcFormat, srcType, image);

            //if (isPowerOf2(image.width) && isPowerOf2(image.height)) {
                gl.generateMipmap(gl.TEXTURE_2D);
            // } else {
            //     gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
            //     gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
            //     gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
            // }

            r.ready = true;
        };
        image.src = url;

        return r
    }
    ////////////////////////////////////////// VERTICES
    
    positionBuffer = gl.createBuffer();
    texCoordBuffer = gl.createBuffer();
    indexBuffer = gl.createBuffer();

    gl.begin = (m) =>
    {
        vertex2f = [];
        tex2f = [];
        indices = [];

        mode = m;
    };

    gl.vertex2f = (x, y) =>
    {
        vertex2f.push(x);
        vertex2f.push(y);

        var vertices = vertex2f.length / 2;
        indices.push(vertices - 1);
    }

    gl.texCoord2f = (s, t) =>
    {
        tex2f.push(s);
        tex2f.push(t);
    }

    gl.end = () =>
    {
        gl.useProgram(shaderProgram);
       
        gl.uniform1i(u_texture0, 0);

        gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
        gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(vertex2f), gl.DYNAMIC_DRAW);
        var error = gl.getError();
        gl.vertexAttribPointer(a_coords, 2, gl.FLOAT, false, 0, 0);
        gl.enableVertexAttribArray(a_coords);

        gl.bindBuffer(gl.ARRAY_BUFFER, texCoordBuffer);
        gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(tex2f), gl.DYNAMIC_DRAW);
        error = gl.getError();
        gl.vertexAttribPointer(a_texcoords, 2, gl.FLOAT, false, 0, 0);
        gl.enableVertexAttribArray(a_texcoords);

        gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indexBuffer);
        gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, new Uint16Array(indices), gl.DYNAMIC_DRAW);
        error = gl.getError();

        var r = multiplyMatrices(pmatrix33f, matrix33f);
        gl.uniformMatrix3fv(u_matrix, false, r);

        gl.drawElements(mode, indices.length, gl.UNSIGNED_SHORT, 0);
    };

    function multiplyMatrices(a, b) {
        var c = [0, 0, 0, 0, 0, 0, 0, 0, 0];
        for (var i = 0; i < 3; ++i)
            for (var j = 0; j < 3; ++j)
                for (var k = 0; k < 3; ++k)
                    c[i*3+j] += a[i*3+k] * b[k*3+j];
        return c;
    }
    gl.multiplyMatrices= multiplyMatrices;

    function multiplyMatrixVertex(a, b) {
        var c = [0, 0, 0];
        for (var i = 0; i < 3; ++i)
            for (var k = 0; k < 3; ++k)
                c[i] += a[i*3+k] * b[k];
        return c;
    }

    

    function applyMatrix(a) {
        if(matrixmode == gl.MODELVIEW) {
            matrix33f = multiplyMatrices(matrix33f, a);
        } else if(matrixmode == gl.PROJECTION) {
            pmatrix33f = multiplyMatrices(pmatrix33f, a);
        }
    }

    gl.MODELVIEW = 0;
    gl.PROJECTION = 1;

    gl.matrixMode = (m) => {
        matrixmode = m;
    }

    gl.loadMatrix3f = (m) => {
        if(matrixmode == gl.MODELVIEW) {
            matrix33f = [m[0], m[1], m[2], m[3], m[4], m[5], m[6], m[7], m[8]];
        } else if(matrixmode == gl.PROJECTION) {
            pmatrix33f = [m[0], m[1], m[2], m[3], m[4], m[5], m[6], m[7], m[8]];
        }
    };

    gl.identity2f = () => {
        gl.loadMatrix3f([1, 0, 0, 0, 1, 0, 0, 0, 1]);
    }

    gl.scale2f = (x, y) =>
    {
        applyMatrix([x, 0, 0, 0, y, 0, 0, 0, 1]);
    };

    gl.translate2f = (x, y) =>
    {
        applyMatrix([1, 0, x, 0, 1, y, 0, 0, 1]);
    };

    gl.rotate2f = (ccw) =>
    {
        applyMatrix([Math.cos(ccw), -Math.sin(ccw), 0, Math.sin(ccw), Math.cos(ccw), 0, 0, 0, 1]);
    };

    gl.multMatrix = (m) => {
        applyMatrix(m);
    }

    let matrices = [];
    let pmatrices = [];

    gl.pushMatrix = () =>
    {
        if(matrixmode == gl.MODELVIEW) {
            matrices.push(matrix33f);
        } else if(matrixmode == gl.PROJECTION) {
            pmatrices.push(matrix33f);
        }
    };

    gl.popMatrix = () =>
    {
        if(matrixmode == gl.MODELVIEW) {
            matrix33f = matrices.pop();
        } else if(matrixmode == gl.PROJECTION) {
            pmatrix33f = pmatrices.pop();
        }
    };

    gl.getTranslate = () => {
        if(matrixmode == gl.MODELVIEW) {
            return [matrix33f[2],matrix33f[5]];
        } else if(matrixmode == gl.PROJECTION) {
            return [pmatrix33f[2],pmatrix33f[5]];
        }
    }

   

    gl.localToWorld = (m, v) => {
        v = v || [0, 0, 1];
        if(!m)
            return multiplyMatrixVertex(matrix33f, v);
        else {
            return multiplyMatrixVertex(m, v);
        }
    }

    gl.getFloatv = (mode) => {
        if(mode == gl.MODELVIEW) {
            return matrix33f;
        } else if(mode == gl.PROJECTION) {
            return pmatrix33f;
        }
    }
}

var tree = {};
var byName = {};
var bones = spine.spineboy.bones;
bones.forEach((x,i) => {
    var node
    if(!x.parent) {
        node = tree = {
            bone: x,
            children: [],
            state: {},
            byIndex: {}
        };
        
        byName[x.name] = tree;
    } else {
        node = {
            bone: x,
            children: [],
            state: {}
        };
        byName[x.name] = node;
        byName[x.parent].children.push(node);
    }

    tree.byIndex[i] = node;
    node.x = x.x || 0;
    node.y = x.y || 0;    
    node.sx = x.scaleX || 1;
    node.sy = x.scaleY || 1;
    node.angle = x.rotation*2*3.14159/360 || 0;
    node.debugLength = x.length || 10;
    node.selected = false;
});
var head = spine.spineboy.skins.default.head["spineboy/head"];

function updateNode(gl, node)
{
    gl.pushMatrix();        

    var {x,y,angle,sx,sy} = node;

    gl.scale2f(sx, sy);
    gl.translate2f(x, y);
    gl.rotate2f(angle);

    node.matrix = gl.getFloatv(gl.MODELVIEW);
    node.worldx = node.matrix[2];
    node.worldy = node.matrix[5];

    node.children.forEach(x => {
        updateNode(gl, x);
    });
    gl.popMatrix();
}

function drawRectangle(gl, left, right, bottom, top) {
    gl.begin(gl.TRIANGLES);
        gl.vertex2f(left,top); gl.texCoord2f(0, 1);
        gl.vertex2f(right,top); gl.texCoord2f(1, 1);
        gl.vertex2f(right,bottom); gl.texCoord2f(1, 0);
        
        gl.vertex2f(left,top); gl.texCoord2f(0, 1);
        gl.vertex2f(right,bottom); gl.texCoord2f(1, 0);
        gl.vertex2f(left,bottom); gl.texCoord2f(0, 0);                    
    gl.end();
}

function drawRectangleCenter(gl, cx, cy, w, h) {
    drawRectangle(gl, cx - w/2, cx + w/2, cy - h/2, cy+h/2);
}

function drawBone(gl, l, s)
{
    s = s || 10;
    
    drawRectangleCenter(gl, 0, 0, s, s);
    drawRectangleCenter(gl, l/2, 0, l, 3);
}

function drawNode(gl, node)
{
    var {debugLength, selected} = node;
    var s = selected ? 20 : 10;

    gl.pushMatrix();
    gl.loadMatrix3f(node.matrix);
    drawBone(gl, debugLength, s);
    //if(node.draw) node.draw(gl);
    gl.popMatrix();

    node.children.forEach(x => {
        drawNode(gl, x);
    });
}
var spineVertices = [];
var spineBones = [];    
function prepareModel()
{
    for(var i = 0; i < head.vertices.length; ++i)
    {
        var count = head.vertices[i]; ++i;
        var bx = 0, by = 0;
        var bones = [];
        for(var j = 0; j < count; ++j)
        {
            var b = byName[
                spine.spineboy.bones[
                    head.vertices[i]
                ].name]; ++i;
            
            var bbx = head.vertices[i]; ++i;
            var bby = head.vertices[i]; ++i;
            var w = head.vertices[i]; ++i;
            bones.push([bbx, bby, b,w]);

            bx += (b.worldx + bbx) * w;
            by += (b.worldy + bby) * w;
        }
        --i;
        spineVertices.push(bx);
        spineVertices.push(by);
        spineBones.push(bones);
    }
    console.log(spine.spineboy);
    console.log(spineBones);
}

function adjustUV(u, v)
{
// spineboy/head
//   rotate: false
//   xy: 571, 582
//   size: 146, 161
//   orig: 146, 161
//   offset: 0, 0
//   index: -1
    var sx = 571/1024;
    var sy = 582/1024;

    return [sx + u*146/1024, sy + v*161/1024];
}

var atlas1Texture;
byName["head"].draw = (gl) => {
    if(atlas1Texture.ready) {
        gl.activeTexture(gl.TEXTURE0);
        gl.bindTexture(gl.TEXTURE_2D, atlas1Texture.texture); 
        
        gl.pushMatrix();
        gl.identity2f();
        for(var i = 0; i < head.triangles.length; ++i)
        {
            var i0 = head.triangles[i]; ++i;
            var i1 = head.triangles[i]; ++i;
            var i2 = head.triangles[i];

            var v0x = spineVertices[i0*2];
            var v0y = spineVertices[i0*2+1];
            var v1x = spineVertices[i1*2];
            var v1y = spineVertices[i1*2+1];
            var v2x = spineVertices[i2*2];
            var v2y = spineVertices[i2*2+1];

            var uv0x = head.uvs[i0*2];
            var uv0y = head.uvs[i0*2+1];
            var uv1x = head.uvs[i1*2];
            var uv1y = head.uvs[i1*2+1];
            var uv2x = head.uvs[i2*2];
            var uv2y = head.uvs[i2*2+1];

            var b0 = spineBones[i0];
            var b1 = spineBones[i1];
            var b2 = spineBones[i2];

            const calc = (vx, vy, b) => {
                b.forEach(x => {
                    var [dx, dy] = gl.localToWorld(x[2].matrix, [x[0],x[1],1]);
                    vx += dx * x[3];
                    vy += dy * x[3];
                });
                return [vx, vy];
            }
            [v0x, v0y] = calc(0, 0, b0);
            [v1x, v1y] = calc(0, 0, b1);
            [v2x, v2y] = calc(0, 0, b2);

            var uv0 = adjustUV(uv0x, uv0y);
            var uv1 = adjustUV(uv1x, uv1y);
            var uv2 = adjustUV(uv2x, uv2y);

            gl.begin(gl.TRIANGLES);
                gl.vertex2f(v0x,v0y); gl.texCoord2f(uv0[0],uv0[1]);
                gl.vertex2f(v1x,v1y); gl.texCoord2f(uv1[0],uv1[1]);
                gl.vertex2f(v2x,v2y); gl.texCoord2f(uv2[0],uv2[1]);
            gl.end();
        }
        gl.popMatrix();
        gl.bindTexture(gl.TEXTURE_2D, gl.noTexture);  
    }
};


let startGLOK;
const startGL = new Promise((ok) => {
    startGLOK = ok;
});
startGL.then(function (rootEl) {    
    var canvas = rootEl.querySelectorAll("canvas")[0];

    var zoomScale = 1;
    var tx = 0, ty= 0;

    let wheelValue = 0;
    let panx = 0;
    let pany = 0;

    var viewConfig = localStorage.getItem('viewConfig');
    if(viewConfig)
    {
        viewConfig = JSON.parse(viewConfig);
        zoomScale = viewConfig.zoomScale || 1;
        tx = viewConfig.tx || 0;
        ty = viewConfig.ty || 0;
    }
    canvas.onmousewheel = function (event){
        var mousex = event.clientX - canvas.offsetLeft;
        var mousey = event.clientY - canvas.offsetTop;
        wheelValue = event.wheelDelta/1200;//n or -n
    }
    let mouseDown = false;
    let lmousex = 0;
    let lmousey = 0;
    canvas.onmousedown = function (event){
        mouseDown = true;
        lmousex = event.clientX - canvas.offsetLeft;
        lmousey = event.clientY - canvas.offsetTop;
        canvas.style = "cursor: grabbing;"
    }
    canvas.onmouseup = function (event){
        mouseDown = false;

        canvas.style = "cursor: default;"
    }
    canvas.onmousemove = function (event){
        if(mouseDown)
        {
            var mousex = event.clientX - canvas.offsetLeft;
            var mousey = event.clientY - canvas.offsetTop;

            panx = (mousex - lmousex) / zoomScale;
            pany = -(mousey - lmousey) / zoomScale;

            lmousex = mousex;
            lmousey = mousey;
        }
    }

    
    var gl = canvas.getContext("webgl");
    immediateMode2D(gl);

    atlas1Texture = gl.newTexture(atlas1);

    ///////////////////////////////////////// DRAW
    var fps = rootEl.querySelectorAll("#fps")[0];
    var lastTimestamp = null;
    var i = Math.random() * 3;
    var maxDT = 0;
    var maxDTi = 0;

    gl.matrixMode(gl.MODELVIEW);
    gl.identity2f();
    updateNode(gl, tree);
    prepareModel();

    var shader = createSkinnedShader(gl, 6);
    var hi = shader.fill(
        head.vertices,
        head.uvs,
        head.triangles
    );
    
    function render(timestamp) {
        if (!lastTimestamp) lastTimestamp = timestamp - 16;
        var dt = (timestamp - lastTimestamp) * 0.001;
        maxDT += dt;
        maxDTi++;
        lastTimestamp = timestamp;

        if ((timestamp % 10) == 0) {
            i = Math.random() * 3;
        }
        
        gl.clearColor(0.0, 0.0, 1, 1.0);
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

        gl.activeTexture(gl.TEXTURE0);
        gl.bindTexture(gl.TEXTURE_2D, gl.noTexture);   

        gl.matrixMode(gl.PROJECTION);
        gl.identity2f();
        gl.translate2f(-1, 1);
        gl.scale2f(zoomScale/400.0, zoomScale/300.0);
        gl.translate2f(tx, ty);
        
        gl.matrixMode(gl.MODELVIEW);
        gl.identity2f();
        updateNode(gl, tree);
        if(atlas1Texture.ready) {
            hi.texture0 = atlas1Texture.texture            
            shader.render(hi, tree.byIndex);
            gl.bindTexture(gl.TEXTURE_2D, gl.noTexture);
        }
        drawNode(gl, tree);


        tx += panx;
        ty += pany;
        zoomScale += wheelValue;

        if(panx || pany ||wheelValue)
        {
            localStorage.setItem('viewConfig', JSON.stringify({
                tx, ty, zoomScale
            }));
        }
        
        wheelValue = 0;
        panx = 0;
        pany = 0;
        
        if(fps)
            fps.innerHTML = `${dt.toFixed(4)} - max: ${(maxDT / maxDTi).toFixed(4)} - fps ${(1 / dt).toFixed(4)} - ${mouseDown} ${panx} `;
        requestAnimationFrame(render);

    };
    requestAnimationFrame(render);
});


var config = {
    content: [{
        type: 'row',
        content:[{
            type: 'component',
            componentName: 'renderCanvas',
            componentState: { label: 'A' }
        },{
            type: 'column',
            content:[{
                type: 'component',
                componentName: 'tree',
                componentState: { label: 'B' }
            }]
        }]
    }]
};
var myLayout = new GoldenLayout(config);
var nodeData = {};
var lastWorldX;
var lastSelected;
function renderItem(root, node, paddingLeft)
{
    paddingLeft = paddingLeft || 0;
    nodeData[node.bone.name] = {
        closed: true
    };
    var worldX = span();
    var item = div(
        button("more", {style:"display:inline-block"}),
        div(node.bone.name, {style:"color:white;display:inline-block"}),
        worldX,
        {style:`padding-left:${paddingLeft}px`}
    );
    item.addEventListener("click", (e) => {
        
        if(lastSelected)
            lastSelected.selected = false;
        
        lastSelected = byName[node.bone.name]
        lastSelected.selected = true;

        lastWorldX = worldX;
    });
    root.appendChild(item);

    

    node.children.forEach(x => {
        renderItem(root, x, paddingLeft + 20);
    });
}
myLayout.registerComponent( 'tree', function( container, componentState ){
    var r = container.getElement().html("");
    r[0].style = "color: white; overflow-y: scroll";
    renderItem(r[0], tree);

    setInterval(function() {
        if(lastSelected)
        {
            lastWorldX.innerText = ` ${lastSelected.worldx.toFixed(2)}; ${lastSelected.worldy.toFixed(2)}`;
        }
    }, 1000);
});
myLayout.registerComponent('renderCanvas', function( container, componentState ){
    var r = container.getElement().html(`<div>
    <div>
        <canvas width="800" height="600"></canvas>
    </div>
    <span id="fps" style="color:white"></span>
</div>` );
    startGLOK(r[0]);
});
myLayout.init();