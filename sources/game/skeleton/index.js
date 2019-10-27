import spine from './spine.json';
import {h3, h2, h1, div, span, button} from 'hyperaxe';
import atlas1 from './atlas1.png';
import atlasTexture from './atlas1.txt';
import 'babel-polyfill';
import '@babel/plugin-proposal-nullish-coalescing-operator';

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
//console.log(vshader);
    const fshader = ` precision mediump float;
uniform sampler2D u_texture0;
varying highp vec2 out_texcoords;

void main() {
    gl_FragColor = texture2D(u_texture0, out_texcoords);// + vec4(0.3, 0.3, 0.3, 0);
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

    shader.fill = (vertices, tex2f, indices, texAtlas, part) => {
        var instance = {
            n: n,
            positionBuffers: [],
            texCoordBuffer: gl.createBuffer(),
            indexBuffer: gl.createBuffer(),
            qtd: indices.length, 
        };

        for(var i = 0;i < tex2f.length; i+=2)
        {
            var [u,v] = adjustUV(
                texAtlas.size, part.xy, part.size,
                tex2f[i+0], tex2f[i+1],
                part.rotate);
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

        if(vertices.length != tex2f.length)
        {
            //adjust vertex buffer
            var bones = [];
            var bonei = {};

            for(var i = 0; i < vertices.length; ++i)
            {
                var count = vertices[i]; ++i;
                for(var j = 0; j < count; ++j)
                {
                    var bi = vertices[i]; ++i;
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
            
            for(var i = 0; i < vertices.length; ++i)
            {
                var count = vertices[i]; ++i;
                var j = 0;

                var assignedBones = Array.from({length: n}, (el, index) => index);
                for(j = 0; j < count; ++j)
                {
                    var bi = bonei[vertices[i]]; ++i;
                    if(!bi) console.error("bone index");
                    
                    var index = assignedBones.indexOf(bi);
                    if (index !== -1) assignedBones.splice(index, 1);

                    var bbx = vertices[i]; ++i;
                    var bby = vertices[i]; ++i;
                    var w = vertices[i]; ++i;

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
        }
        else 
        {
            var buffer = vbuffer[0];
            for(var i = 0; i < vertices.length; ++i)
            {
                var bbx = vertices[i]; ++i;
                var bby = vertices[i];
                buffer.push(bbx);
                buffer.push(bby);
                buffer.push(1);
            }

            for(var j = 1; j < vbuffer.length; ++j)
            {
                var buffer = vbuffer[j];
                for(var i = 0; i < vertices.length; ++i)
                {
                    buffer.push(0);
                    buffer.push(0);
                    buffer.push(0);
                }
            }

            //instance.bones = [uniqueBone];
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

    shader.render = (instance, bones, texture) => {
        var {positionBuffers, 
            texCoordBuffer, 
            indexBuffer,
            qtd} = instance;

        gl.useProgram(shaderProgram);

        gl.activeTexture(gl.TEXTURE0);
        gl.bindTexture(gl.TEXTURE_2D, texture);
        gl.uniform1i(shader.u_texture0, texture);

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

        gl.enable(gl.BLEND);
        gl.blendFunc(gl.ONE, gl.ONE_MINUS_SRC_ALPHA);

        gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indexBuffer);
        gl.drawElements(gl.TRIANGLES, qtd, gl.UNSIGNED_SHORT, 0);

        gl.disable(gl.BLEND);
        gl.bindTexture(gl.TEXTURE_2D, gl.noTexture);
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

        r.p = new Promise((ok, rej) => {
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
                ok(image);
            };
            image.onerror = function(e) {
                rej(e);
            };
            image.src = url;
        });

        return r
    }

    gl.newAtlasTexture = async (url, urls) => {        
        var res = await fetch(url);
        var txt = await res.text();
        
        var lines = txt.split("\n");
        var iline = 0;
        var file = {
            parts: {}
        };
        file.name = lines[iline].trim(); ++iline;
        var tex = gl.newTexture(urls[file.name]);
        await tex.p;
        file.texture = tex.texture;
        while(lines[iline].indexOf(":") >= 0)
        {
            var propParts = lines[iline].split(":");                
            file[propParts[0].trim()] = propParts[1].trim();

            ++iline;
        }
        file.size = file.size.split(",").map(x => parseInt(x));

        while(iline < lines.length)
        {
            var part = {};
            part.name = lines[iline].trim(); ++iline;
            file.parts[part.name] = part;
            while(iline < lines.length && lines[iline].indexOf(":") >= 0)
            {
                var propParts = lines[iline].split(":");                
                part[propParts[0].trim()] = propParts[1].trim();

                ++iline;
            }
            part.xy = part.xy.split(",").map(x => parseInt(x));
            part.size = part.size.split(",").map(x => parseInt(x));
            part.rotate = part.rotate.toLowerCase() == "true";
        }
        
        var atlas = {
            files: [file]
        };

        return atlas;
    };
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

    //https://math.stackexchange.com/questions/13150/extracting-rotation-scale-values-from-2d-transformation-matrix
    function getTransformations(m)
    {
        var tx = m[2];
        var ty = m[5];
        
        var sx = Math.sign(m[0]) * Math.sqrt(m[0]*m[0] + m[1]*m[1]);
        var sy = Math.sign(m[4]) * Math.sqrt(m[3]*m[3] + m[4]*m[4]);

        var angle = Math.atan2(m[3], m[4]);

        return [tx, ty, sx, sy, angle];
    }

    gl.getTransformations = () => {
        if(matrixmode == gl.MODELVIEW) {
            return getTransformations(matrix33f);
        } else if(matrixmode == gl.PROJECTION) {
            return getTransformations(pmatrix33f);
        }
    }

    gl.patchMatrix = (p) => {
        if(!p) return;
        
        var m ;
        if(matrixmode == gl.MODELVIEW) {
            m = matrix33f;
        } else if(matrixmode == gl.PROJECTION) {
            m = pmatrix33f;
        }
        switch(p)
        {
            case "normal": return;
            case "onlyTranslation": {
                var tx = m[2];
                var ty = m[5];
                gl.loadMatrix3f([1, 0, tx, 0, 1, ty, 0, 0, 1]);
                return;
            };
            case "noRotationOrReflection": {
                var tx = m[2];
                var ty = m[5];
                var sx = Math.sign(m[0]) * Math.sqrt(m[0]*m[0] + m[1]*m[1]);
                //if(sx < 0) sx = 0;
                var sy = Math.sign(m[4]) * Math.sqrt(m[3]*m[3] + m[4]*m[4]);
                //if(sy < 0) sy = 0;
                gl.loadMatrix3f([sx, 0, tx, 0, sy, ty, 0, 0, 1]);
                return;
            };
            case "noScale": {
                var tx = m[2];
                var ty = m[5];
                var angle = Math.atan2(m[3], m[4]);
                gl.loadMatrix3f([1, 0, tx, 0, 1, ty, 0, 0, 1]);
                gl.rotate2f(angle);
                return;
            };
            case "noScaleOrReflection": {
                var tx = m[2];
                var ty = m[5];
                var angle = Math.atan2(m[3], m[4]);
                gl.loadMatrix3f([1, 0, tx, 0, 1, ty, 0, 0, 1]);
                gl.rotate2f(angle);
                return;
            }
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

function defaultNodeValues()
{
    return {x:0, y: 0, angle: 0, sx: 1, sy:1}
}

function inheritTransform(node)
{
    var bone = node.bone;
    if(!bone.transform) return {noRotation: false};
    if(bone.transform == "noRotationOrReflection")
        return {noRotation: true}
    else if(bone.transform == "onlyTranslation")
        return {noRotation: true}

    return {noRotation: false};
}

function updateNode(gl, node, parent)
{
    var pworld = (parent && parent.world) || defaultNodeValues();
    var px = pworld.x;
    var py = pworld.y;
    var psx = pworld.sx;
    var psy = pworld.sy;
    var pangle = pworld.angle;
        
    var {x, y, angle, sx, sy} = node;
    var {noRotation} = inheritTransform(node);
    
    var cangle = pangle;
    var cosa = Math.cos(pangle);
    var sina = Math.sin(pangle);
    var ctx = px + (x*cosa - y*sina);
    var cty = py + (x*sina + y*cosa);

    if(noRotation) pangle = 0;

    cangle = pangle + angle;
    cosa = Math.cos(cangle);
    sina = Math.sin(cangle);

    node.world = {
        x: ctx,
        y: cty,
        angle: cangle,
        sx: psx * sx,
        sy: psy * sy
    };

    node.matrix = [
        cosa, -sina, ctx,
        sina,  cosa, cty,
        0, 0, 1
    ];

    node.children.forEach(x => {
        updateNode(gl, x, node);
    });
}

function applyTransformConstraints(skeleton, obj)
{
    obj.transform.forEach(x => {
        var target = skeleton.byName[x.target];
        var dx = x.x || 0;
        var dy = x.y || 0;
        x.bones.forEach(xx => {
            var bone = skeleton.byName[xx];

            bone.world.x = 
                (1-x.translateMix)*(bone.world.x) +
                x.translateMix*(target.world.x + dx);
            bone.world.y = 
                (1-x.translateMix)*(bone.world.y) +
                x.translateMix*(target.world.y + dy);
            bone.matrix[2] = bone.world.x;
            bone.matrix[5] = bone.world.y;
        });
    });
}

function updateNode2(gl, node, parent)
{
    gl.pushMatrix();        
    //gl.patchMatrix(node.bone.transform);

    var {px = x, py = y, pangle = angle, psx = sx, psy = sy} 
        = (node.parent && node.parent.world) || defaultNodeValues();
    var {x, y, angle, sx, sy} = node;

    node.world = {
        x: px + x,
        y: py + y,
        angle: pangle + angle,
        sx: px * sx,
        sy: py * sy
    };

    //gl.scale2f(sx, sy);
    gl.translate2f(x, y);
    gl.rotate2f(angle);
    

    node.matrix = gl.getFloatv(gl.MODELVIEW);

    node.children.forEach(x => {
        updateNode(gl, x, node);
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
    drawRectangleCenter(gl, l/2, 0, l, 3 * s/10);
}

function drawNode(gl, node)
{
    var {debugLength, selected} = node;
    var s = selected ? 20 : 10;

    gl.pushMatrix();
    gl.loadMatrix3f(node.matrix);
    drawBone(gl, debugLength, s);
    gl.popMatrix();

    node.children.forEach(x => {
        drawNode(gl, x);
    });
}

function prepareSkeleton(model)
{
    var tree = {};
    var byName = {};
    var bones = model.bones;
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
        node.i = i;
        node.x = x.x || 0;
        node.y = x.y || 0;    
        node.sx = x.scaleX || 1;
        node.sy = x.scaleY || 1;
        node.angle = x.rotation*2*3.14159/360.0 || 0;
        node.debugLength = x.length || 10;
        node.selected = false;
    });

    return {
        root: tree,
        byName,
        byIndex: tree.byIndex
    };
}

function prepareSlot(skeleton, model, name, shader, texAtlas, slot)
{
    var attachment = model.skins[name][slot.name][slot.attachment];
    var tex = texAtlas.parts[slot.attachment];
    if(!attachment.type) {
        var x = attachment.x || 0;
        var y = attachment.y || 0;
        var sx = attachment.scaleX || 1;
        var sy = attachment.scaleY || 1;
        var rotation = attachment.rotation || 0;
        var w = attachment.width;
        var h = attachment.height;
        var color = attachment.color;

        rotation *= 2*3.14159/360;

        function transformedVertex([vx,vy])
        {
            //var ix = x*Math.cos(rotation) - y*Math.sin(rotation);
            //var iy = x*Math.sin(rotation) + y*Math.cos(rotation);
            var ix = x;
            var iy = y;
            
            var ivx = (vx*w*sx)*Math.cos(rotation) - (vy*h*sy)*Math.sin(rotation);
            var ivy = (vx*w*sx)*Math.sin(rotation) + (vy*h*sy)*Math.cos(rotation);
            return [
                ix + ivx,
                iy + ivy
            ];
        }

        var a = [-0.5,-0.5];
        var b = [ 0.5,-0.5];
        var c = [ 0.5, 0.5];
        var d = [-0.5, 0.5];

        // var a = [0,0];
        // var b = [1,0];
        // var c = [1,1];
        // var d = [0,1];

        var ta = transformedVertex(a);
        var tb = transformedVertex(b);
        var tc = transformedVertex(c);
        var td = transformedVertex(d);

        var vertices = [
            ta[0], ta[1],
            tb[0], tb[1],
            tc[0], tc[1],
            td[0], td[1],
        ];
        var uvs;
        if(tex.rotate) {
            // ok by gun attachment
            uvs = [
                1, 1,
                1, 0,
                0, 0,
                0, 1,
            ];
        } else {
            uvs = [
                0, 1,
                1, 1,
                1, 0,
                0, 0,
            ];
        }
        var triangles = [0, 1, 2, 0, 2, 3];
        var i = shader.fill(
            vertices,
            uvs,
            triangles,
            texAtlas,
            tex,
            slot,
            attachment
        );
        if(!i.bones || i.bones.length == 0)
            i.bones = [skeleton.byName[slot.bone].i];
        return i;
    }
    else if(attachment.type === "mesh")
    {
        var i = shader.fill(
            attachment.vertices,
            attachment.uvs,
            attachment.triangles,
            texAtlas,
            tex,
            slot,
            attachment
        );
        if(!i.bones || i.bones.length == 0)
            i.bones = [skeleton.byName[slot.bone].i];
        return i;
    }
    else {
        console.log("unknown attachment", slot, attachment)
    }
}

function prepareModel(model, name, shader, texture, skeleton)
{
    var skin = {};
    var instances = [];

    const add = (skeleton, slot) =>
    {
        var i = prepareSlot(skeleton, model, name, shader, texture, slot);
        if(i) instances.push(i);
        return i;
    };
    
    for(var i = 0;i < model.slots.length; ++i)
    {
        var slot = model.slots[i];
        if(slot.attachment)
            add(skeleton, slot);
    }
    //add(skeleton, model.slots[21]);
    //add(skeleton, model.slots[24]);
    //add(skeleton, model.slots[28]);
    //add(skeleton, model.slots[31]);

    skin.render = () =>
    {
        instances.forEach(x => {            
            shader.render(x, skeleton.byIndex, texture.texture);
        });        
    }

    return skin;
}

function adjustUV(size, xy, wh, u, v, rotate)
{
    var sx = xy[0]/size[0];
    var sy = xy[1]/size[1];
    var uv = [sx + u*wh[0]/size[0], sy + v*wh[1]/size[1]];
    return uv;
    //return rotate ? uv : [uv[1], uv[0]];
}

let startGLOK;
const startGL = new Promise((ok) => {
    startGLOK = ok;
});
var skeleton = prepareSkeleton(spine.spineboy);
console.log(spine);
startGL.then(async (rootEl) => {    
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

            var scale = 1/zoomScale * 5;
            panx = (mousex - lmousex) * scale;
            pany = -(mousey - lmousey) * scale;

            lmousex = mousex;
            lmousey = mousey;
        }
    }
    
    var gl = canvas.getContext("webgl");
    immediateMode2D(gl);

    ///////////////////////////////////////// DRAW
    var fps = rootEl.querySelectorAll("#fps")[0];
    var lastTimestamp = null;
    var i = Math.random() * 3;
    var maxDT = 0;
    var maxDTi = 0;

    gl.matrixMode(gl.MODELVIEW);
    gl.identity2f();
    updateNode(gl, skeleton.root);
    applyTransformConstraints(skeleton, spine.spineboy);
    
    var atlas = await gl.newAtlasTexture(atlasTexture, {
        "atlas1.png": atlas1
    });
    var shader = createSkinnedShader(gl, 6);
    
    var skin = prepareModel(spine.spineboy, 
        "default", 
        shader, 
        atlas.files[0],
        skeleton);
    
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
        updateNode(gl, skeleton.root);
        applyTransformConstraints(skeleton, spine.spineboy);
        skin.render();
        drawNode(gl, skeleton.root);

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


function localStorageObject(key) {
    var item = localStorage.getItem(key) || "{}";
    item = JSON.parse(item);
    let normalObj = {
        set: function(obj, prop, value) {
            obj[prop] = value;

            var json = JSON.stringify(item);
            localStorage.setItem(key, json);

            return true;
        }
    };
    let proxy = {
        set: function(obj, prop, value) {
            obj[prop] = value;

            var json = JSON.stringify(item);
            localStorage.setItem(key, json);

            return true;
        },
        get: function(obj, prop) {
            var target = obj[prop];
            if(!target) obj[prop] = target = {};

            return new Proxy(target, normalObj);
        }
    };
    return new Proxy(item, proxy);
}

var goldenLayoutStore = localStorageObject('goldenLayout');
var config = {
    content: [{
        type: 'row',
        content:[{
            type: 'component',
            componentName: 'renderCanvas',
        },{
            type: 'column',
            content:[{
                type: 'component',
                componentName: 'tree',
                componentState: { storeId: 'nodesTree' }
            },{
                type: 'component',
                componentName: 'node',
                componentState: { storeId: 'selectedNode' }
            }]
        }]
    }]
};
var myLayout = new GoldenLayout(config);
var nodeData = {};
var lastSelected;
function renderItem(root, node, paddingLeft, emitf, state)
{
    paddingLeft = paddingLeft || 0;
    nodeData[node.bone.name] = {
        closed: true
    };
    var item = div(
        button("more", {style:"display:inline-block"}),
        div(node.bone.name, {style:"color:white;display:inline-block"}),
        {style:`padding-left:${paddingLeft}px`}
    );
    item.addEventListener("click", (e) => {
        
        if(lastSelected)
            lastSelected.selected = false;
        
        lastSelected = skeleton.byName[node.bone.name]
        lastSelected.selected = true;
        state.lastSelected = lastSelected.i;

        emitf("nodeSelected", lastSelected);
    });
    root.appendChild(item);

    node.children.forEach(x => {
        renderItem(root, x, paddingLeft + 20, emitf, state);
    });
};

myLayout.registerComponent('tree', function( container, componentState ){
    var state = goldenLayoutStore[componentState.storeId];

    var r = container.getElement().html("");
    r[0].style = "color: white; overflow-y: scroll";
    var h = container.layoutManager.eventHub;
    renderItem(r[0], skeleton.root, null, (name, arg) => {
        h.emit(name, arg);
    }, state);

    
});

function when(container, eventName, f)
{
    var r = container.getElement().html("")[0];
    var h = container.layoutManager.eventHub;
    var lastArg;
    var render = (arg) =>
    {
        lastArg = arg;
        let els = f(arg);
        
        r.innerHTML = "";
        r.append(...els);
    }
    var update = () => render(lastArg);
    h.on("nodeSelected", render);
    return {
        root: r,
        render,
        update
    };
}

myLayout.registerComponent('node', function(container, componentState) {
    let {root, update} = when(container, "nodeSelected", function* (node) {        
        state.current = node.i;
        yield h3(node.bone.name);
        yield h1("Local");
        yield div(`x: ${node.x}`);
        yield div(`y: ${node.y}`);
        yield div(`scale x: ${node.sx}`);
        yield div(`scale y: ${node.sy}`);
        yield div(`angle: ${node.angle} (${node.angle * 180/3.14159})`);
        yield div(`transform: ${node.bone.transform}`);
        if(node.world) {
            yield h2("World");
            yield div(`x: ${node.world.x}`);
            yield div(`y: ${node.world.y}`);    
            yield div(`angle: ${node.world.angle} (${node.world.angle * 180/3.14159})`);
        }
        if(node.matrix)
        {
            yield div(JSON.stringify(node.matrix, null, 4));
        }
    });
    root.style = "color: white; overflow-y: scroll";

    var state = goldenLayoutStore[componentState.storeId];
    var node = skeleton.byIndex[state.current];
    var h = container.layoutManager.eventHub;
    h.emit("nodeSelected", node);

    setInterval(update, 1000);
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