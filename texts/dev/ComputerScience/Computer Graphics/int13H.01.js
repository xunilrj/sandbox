function getGL() {
    var canvas = document.getElementsById("screen");
    return [canvas.getContext("webgl"), canvas.width, canvas.height];
}

function createRectangle(){
    // Vertices
    var positions = new Float32Array([
        -1.0, -1.0,
        +1.0, -1.0,
        +1.0, +1.0,
        -1.0, +1.0,
    ]);
    var verticesBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, verticesBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, positions, gl.STATIC_DRAW);

    // Triangles
    var indices = new Uint16Array([0, 1, 2, 0, 2, 3]);
    var indicesBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indicesBuffer);
    gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, indices, gl.STATIC_DRAW);
    return [verticesBuffer, indicesBuffer];
}

function createShaders(gl, vname, fname) {
    var vshader = document.getElementById("vshader").innerText;
    var vsshader = gl.createShader(gl.VERTEX_SHADER);
    gl.shaderSource(vsshader, vshader);
    gl.compileShader(vsshader);
    if (!gl.getShaderParameter(vsshader, gl.COMPILE_STATUS)) {
        console.log(gl.getShaderInfoLog(vsshader));
    }

    var fshader = document.getElementById("fshader").innerText;
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
    
    var a_coords = gl.getAttribLocation(shaderProgram, 'a_coords');

    return [vshader, fshader, a_coords];
}

function draw(gl, verticiesBuffer, indicesBuffer, shaderProgram, a_coords) {
    gl.useProgram(shaderProgram);

    gl.bindBuffer(gl.ARRAY_BUFFER, verticiesBuffer);
    gl.vertexAttribPointer(a_coords, 2, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(a_coords);  

    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indicesBuffer);
    gl.drawElements(gl.TRIANGLES, 6, gl.UNSIGNED_SHORT, 0);
}