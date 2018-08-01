const canvas = document.getElementsByTagName("canvas")[0];
const gl = canvas.getContext("webgl");


///////////////////////////////////////////// SHADERS
const vshader = `
attribute vec2 a_coords;
attribute vec2 a_texcoords;

varying highp vec2 out_texcoords;
void main() {
  gl_Position = vec4(a_coords, 0.0, 1.0);   
  out_texcoords = a_texcoords;
}
`;
const fshader = `
precision mediump float;

uniform sampler2D u_texture0;
varying highp vec2 out_texcoords;

void main() {
   gl_FragColor = texture2D(u_texture0, out_texcoords);
}
`;
const vsshader = gl.createShader(gl.VERTEX_SHADER);
gl.shaderSource(vsshader, vshader);
gl.compileShader(vsshader);
if (!gl.getShaderParameter(vsshader, gl.COMPILE_STATUS)) {
  console.log(gl.getShaderInfoLog(vsshader));
}
const fsshader = gl.createShader(gl.FRAGMENT_SHADER);
gl.shaderSource(fsshader, fshader);
gl.compileShader(fsshader);
if (!gl.getShaderParameter(fsshader, gl.COMPILE_STATUS)) {
  console.log(gl.getShaderInfoLog(fsshader));
}

const shaderProgram = gl.createProgram();
gl.attachShader(shaderProgram, vsshader);
gl.attachShader(shaderProgram, fsshader);
gl.linkProgram(shaderProgram);
const a_coords = gl.getAttribLocation(shaderProgram, 'a_coords');
const a_texcoords = gl.getAttribLocation(shaderProgram, 'a_texcoords');
const u_texture0 = gl.getUniformLocation(shaderProgram, 'u_texture0'),

////////////////////////////////////////////// TEXTURE
const level = 0;
const internalFormat = gl.RGBA;
const width = 640;
const height = 480;
const border = 0;
const srcFormat = gl.RGBA;
const srcType = gl.UNSIGNED_BYTE;
const pixel = new Uint8Array(4 * width * height);
for (var x = 0; x < width * height * 4; x += 4) {
  //for(var y = 0;y < height;++y){
  pixel[x + 0] = 255;
  pixel[x + 3] = 255;
  //}
}


//console.log(pixel)
const texture = gl.createTexture();
gl.bindTexture(gl.TEXTURE_2D, texture);
//gl.texParameteri(gl.TEXTURE_2D, gl.EXTURE_MAG_FILTER, gl.NEAREST);
gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
gl.texImage2D(gl.TEXTURE_2D, level, internalFormat,
  width, height, border, srcFormat, srcType,
  pixel);
console.log(gl.getError());
////////////////////////////////////////// VERTICES
const positions = new Float32Array([
  -1.0, -1.0,
  +1.0, -1.0,
  +1.0, +1.0,
  -1.0, +1.0,
]);
const positionBuffer = gl.createBuffer();
gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
gl.bufferData(gl.ARRAY_BUFFER, positions, gl.STATIC_DRAW);

const texCoord = new Float32Array([
  +0.0, +0.0,
  +1.0, +0.0,
  +1.0, +1.0,
  +0.0, +1.0,
]);
const texCoordBuffer = gl.createBuffer();
gl.bindBuffer(gl.ARRAY_BUFFER, texCoordBuffer);
gl.bufferData(gl.ARRAY_BUFFER, texCoord, gl.STATIC_DRAW);

const indices = new Uint16Array([0, 1, 2, 0, 2, 3]);
const indexBuffer = gl.createBuffer();
gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indexBuffer);
gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, indices, gl.STATIC_DRAW);

///////////////////////////////////////// DRAW
const fps = document.getElementById("fps");
const lastTimestamp = null
function render(timestamp) {
  if (!lastTimestamp) lastTimestamp = timestamp - 16;
  const dt = (timestamp - lastTimestamp) * 0.001;
  lastTimestamp = timestamp;

  const i = Math.random() * 3;
  for (var x = 0; x < width*height*4; x += 4) { 
    pixel[x+0] = 0;   
    pixel[x+1] = 0;   
    pixel[x+2] = 0;    
    if(i< 1) pixel[x + 0] = 255;
    else if(i< 2) pixel[x + 1] = 255;
    else pixel[x + 2] = 255;
    pixel[x + 3] = 255;    
  }
  gl.bindTexture(gl.TEXTURE_2D, texture);
  gl.texImage2D(gl.TEXTURE_2D, level, internalFormat,
    width, height, border, srcFormat, srcType,
    pixel);
  //fps.innerHTML = gl.getError()
  gl.clearColor(0.0, 0.0, 0.0, 1.0);
  gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

  gl.useProgram(shaderProgram);
  gl.activeTexture(gl.TEXTURE0);
  gl.bindTexture(gl.TEXTURE_2D, texture);
  gl.uniform1i(u_texture0, 0);

  gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
  gl.vertexAttribPointer(a_coords, 2, gl.FLOAT, false, 0, 0);
  gl.enableVertexAttribArray(a_coords);

  gl.bindBuffer(gl.ARRAY_BUFFER, texCoordBuffer);
  gl.vertexAttribPointer(a_texcoords, 2, gl.FLOAT, false, 0, 0);
  gl.enableVertexAttribArray(a_texcoords);

  gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indexBuffer);
  gl.drawElements(gl.TRIANGLES, 6, gl.UNSIGNED_SHORT, 0);

  fps.innerHTML = dt.toFixed(4);
  requestAnimationFrame(render);

};
requestAnimationFrame(render);