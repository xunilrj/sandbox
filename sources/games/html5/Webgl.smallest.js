const canvas = document.getElementsByTagName("canvas")[0];
const gl = canvas.getContext("webgl");

const vshader = `
attribute vec2 a_coords;
void main() {
   gl_Position = vec4(a_coords, 0.0, 1.0);   
}
`;
const fshader = `
precision mediump float;
void main() {
   gl_FragColor = vec4(1.0,0,0,1);
}
`;
const vsshader = gl.createShader(gl.VERTEX_SHADER);
gl.shaderSource(vsshader, vshader);
gl.compileShader(vsshader);
const fsshader = gl.createShader(gl.FRAGMENT_SHADER);
gl.shaderSource(fsshader, fshader);
gl.compileShader(fsshader);

const shaderProgram = gl.createProgram();
gl.attachShader(shaderProgram, vsshader);
gl.attachShader(shaderProgram, fsshader);
gl.linkProgram(shaderProgram);

gl.clearColor(0.0, 0.0, 0.0, 1.0);
gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

const positionBuffer = gl.createBuffer();
gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);

const positions = [
  -1, -1.0,
  1.0, -1.0,
  1.0, 1.0,
  -1.0, 1.0,
]

gl.bufferData(gl.ARRAY_BUFFER,
  new Float32Array(positions),
  gl.STATIC_DRAW);

const indexBuffer = gl.createBuffer();
gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indexBuffer);

const indices = [0, 1, 2, 0, 2, 3];
gl.bufferData(gl.ELEMENT_ARRAY_BUFFER,
  new Uint16Array(indices),
  gl.STATIC_DRAW);


const numComponents = 2;
const type = gl.FLOAT;
const normalize = false;
const stride = 0;
const offset = 0;
gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
gl.vertexAttribPointer(
  positionBuffer,
  numComponents,
  type,
  normalize,
  stride,
  offset);
gl.enableVertexAttribArray(
  positionBuffer);

gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indexBuffer);
gl.useProgram(shaderProgram);

{
  const vertexCount = 6;
  const type = gl.UNSIGNED_SHORT;
  const offset = 0;
  gl.drawElements(gl.TRIANGLES, vertexCount, type, offset);
}