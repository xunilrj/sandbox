import * as THREE from 'three';
import threeorbitcontrols from 'three-orbit-controls';
import { randomGenerator } from './randomGenerator.js';
import { perlinGenerator } from './perlinGenerator.js';
import mathwasmUrl from 'url:./.build/math.wasm';
import { load } from './.build/math.js';

let GLDEBUG = true;
let MATH;

function line(scene, a, b, color) {
  var material = new THREE.LineBasicMaterial({ color });

  var points = [];
  points.push(new THREE.Vector3(a[0], a[1], a[2]));
  points.push(new THREE.Vector3(b[0], b[1], b[2]));

  var geometry = new THREE.BufferGeometry().setFromPoints(points);
  var line = new THREE.Line(geometry, material);
  scene.add(line);
}

function addOriginAxis(scene) {
  line(scene, [0, 0, 0], [1000, 0, 0], 0xff0000);
  line(scene, [0, 0, 0], [0, 1000, 0], 0x00ff00);
  line(scene, [0, 0, 0], [0, 0, 1000], 0x0000ff);
}

function add4Spheres(scene) {
  var sgeometry = new THREE.SphereGeometry(1, 32, 32);
  var smateriala = new THREE.MeshBasicMaterial({ color: 0xff0000 });
  var smaterialb = new THREE.MeshBasicMaterial({ color: 0x00ff00 });
  var smaterialc = new THREE.MeshBasicMaterial({ color: 0x0000ff });
  var s1 = new THREE.Mesh(sgeometry, smateriala); if (GLDEBUG) { scene.add(s1); }
  var s2 = new THREE.Mesh(sgeometry, smaterialb); if (GLDEBUG) { scene.add(s2); }
  var s3 = new THREE.Mesh(sgeometry, smaterialc); if (GLDEBUG) { scene.add(s3); }
  //var s4 = new THREE.Mesh(sgeometry, smaterial); if(GLDEBUG) { scene.add(s4); }
  return [s1, s2, s3/*,s4*/];
}
let s1, s2, s3;
let ngeom;

function addNormal(scene, f, to) {
  var material = new THREE.LineBasicMaterial({ color: 0xffffff });

  var points = [];
  points.push(new THREE.Vector3(f[0], f[1], f[2]));
  points.push(new THREE.Vector3(to[0], to[1], to[2]));

  var geometry = new THREE.BufferGeometry().setFromPoints(points);
  var line = new THREE.Line(geometry, material);
  if (GLDEBUG) { scene.add(line); }

  return geometry;
}

function heightmapAt(terrain, x, y) {
  let sw = terrain.geometry.parameters.width / terrain.geometry.parameters.widthSegments;
  let sh = terrain.geometry.parameters.height / terrain.geometry.parameters.heightSegments;

  let minx = terrain.position.x - (terrain.geometry.parameters.width / 2);
  let miny = terrain.position.y - (terrain.geometry.parameters.height / 2);

  let maxx = terrain.position.x + (terrain.geometry.parameters.width / 2);
  let maxy = terrain.position.y + (terrain.geometry.parameters.height / 2);

  let xstride = terrain.geometry.parameters.widthSegments + 1;
  let ystride = terrain.geometry.parameters.heightSegments + 1;

  let parray = terrain.geometry.attributes.position.array;
  let narray = terrain.geometry.attributes.normal.array;

  //float tiles
  let ftx = (x - minx) / sw;
  let fty = terrain.geometry.parameters.heightSegments - ((y - miny) / sh);

  //integer tiles
  let itx_min = Math.floor(ftx);
  let itx_max = itx_min + 1;
  let ity_min = Math.floor(fty);
  let ity_max = ity_min + 1;

  //percentual across tile
  let px = ftx - itx_min;
  let py = fty - ity_min;

  let pa, pb, pc;
  let na, nb, nc;


  na = ity_min * xstride * 3 + itx_max * 3;
  pa = parray[na + 2];
  nb = ity_max * xstride * 3 + itx_min * 3;
  pb = parray[nb + 2];

  if (px + py < 1.0) {
    nc = ity_min * xstride * 3 + itx_min * 3;
    pc = parray[nc + 2];
  }
  else {
    nc = ity_max * xstride * 3 + itx_max * 3;
    pc = parray[nc + 2];
  }

  let vec3 = THREE.Vector3;
  let w = 0.333333333;
  let n = new vec3()
    .addScaledVector(new vec3(narray[na + 0], narray[na + 1], narray[na + 2]), w)
    .addScaledVector(new vec3(narray[nb + 0], narray[nb + 1], narray[nb + 2]), w)
    .addScaledVector(new vec3(narray[nc + 0], narray[nc + 1], narray[nc + 2]), w);

  let intersection = MATH.intersection_ray_triangle(
    x, y, 100,
    0, 0, -1,
    parray[na + 0], parray[na + 1], parray[na + 2],
    parray[nb + 0], parray[nb + 1], parray[nb + 2],
    parray[nc + 0], parray[nc + 1], parray[nc + 2]
  );
  let coords = MATH.barycentric_coords(intersection.u, intersection.v,
    parray[na + 0], parray[na + 1], parray[na + 2],
    parray[nb + 0], parray[nb + 1], parray[nb + 2],
    parray[nc + 0], parray[nc + 1], parray[nc + 2])

  x = coords.x;
  y = coords.y;
  let z = coords.z;

  if (GLDEBUG) {

    s1.position.set(parray[na + 0], parray[na + 1], parray[na + 2]);
    s2.position.set(parray[nb + 0], parray[nb + 1], parray[nb + 2]);
    s3.position.set(parray[nc + 0], parray[nc + 1], parray[nc + 2]);

    ngeom.attributes.position.setXYZ(0, x, y, z);
    ngeom.attributes.position.setXYZ(1, x + n.x, y + n.y, z + n.z);
    ngeom.attributes.position.needsUpdate = true;
  }

  return [
    [x, y, z],
    [n.x, n.y, n.z],
    [ity_min * xstride * 3 + itx_min * 3 + 2],
    [na, nb, nc]
  ];
}

function setheightmapAt(terrain, obj, offset = [0, 0, 0]) {
  let [[x, y, z], normal] = heightmapAt(terrain, obj.position.x, obj.position.y);

  obj.position.set(x + offset[0], y + offset[1], z + offset[2]);
}

function erosion(terrain) {
  let maxx = terrain.position.x + (terrain.geometry.parameters.width / 2);
  let maxy = terrain.position.y + (terrain.geometry.parameters.height / 2);
  let minx = terrain.position.x - (terrain.geometry.parameters.width / 2);
  let miny = terrain.position.y - (terrain.geometry.parameters.height / 2);

  let parray = terrain.geometry.attributes.position.array;
  let xstride = terrain.geometry.parameters.widthSegments + 1;
  let ystride = terrain.geometry.parameters.heightSegments + 1;

  let x = minx + (Math.random() * terrain.geometry.parameters.width);
  let y = miny + (Math.random() * terrain.geometry.parameters.height);
  let pos = [x, y];
  let speed = [0, 0];
  let volume = 1.0;
  let sediment = 0;
  let sedimentRate = 1;

  let density = 1;
  let friction = 0.1;

  let evaporation = 0.05;

  let dead = false;

  const init = () => {
    let x = minx + (Math.random() * terrain.geometry.parameters.width);
    let y = miny + (Math.random() * terrain.geometry.parameters.height);

    pos = [x, y];
    speed = [0, 0];

    volume = 1;
    sediment = 0;

    dead = false;
  };

  return {
    step: () => {
      if (dead) { init(); }

      let dt = 0.16;
      let [[x1, y1, z1], n1, [i0], [na, nb, nc]] = heightmapAt(terrain, pos[0], pos[1]);
      let [[x2, y2, z2], n2] = heightmapAt(terrain,
        Math.floor(pos[0]), Math.floor(pos[1]));

      if (isNaN(z2)) z2 = z1;

      if (GLDEBUG) {
        s1.position.set(x1, y1, z1);
        s2.position.set(x2, y2, z2);

        ngeom.attributes.position.setXYZ(0, x1, y1, z1);
        ngeom.attributes.position.setXYZ(1, x1 + n1[0], y1 + n1[1], z1 + n1[2]);
        ngeom.attributes.position.needsUpdate = true;
      }

      let mass = volume * density;
      speed[0] += n1[0] / mass;
      speed[1] += n1[1] / mass;

      pos[0] += speed[0] * dt;
      pos[1] += speed[1] * dt;

      speed[0] *= (1 - friction) * dt;
      speed[1] *= (1 - friction) * dt;

      let len_speed = Math.sqrt((speed[0] * speed[0]) + (speed[1] * speed[1]));
      let c_eq = volume * len_speed * (z2 - z1);
      if (c_eq < 0) c_eq = 0;
      let cdiff = c_eq - sediment;
      let sedimentDelta = dt * sedimentRate * cdiff;
      sediment += sedimentDelta;

      parray[na + 2] -= sedimentDelta * volume / 3;
      parray[nb + 2] -= sedimentDelta * volume / 3;
      parray[nc + 2] -= sedimentDelta * volume / 3;

      volume -= evaporation * dt;

      if (!(pos[0] >= minx && pos[0] <= maxx)) { dead = true; }
      if (!(pos[1] >= miny && pos[1] <= maxy)) { dead = true; }
      if (volume <= 0.1) { dead = true; }

      if (dead) {
        if (!isNaN(sedimentDelta)) {
          parray[na + 2] += sediment / 3;
          parray[nb + 2] += sediment / 3;
          parray[nc + 2] += sediment / 3;
        }
      }

      //console.log(pos, speed);

      terrain.geometry.attributes.position.needsUpdate = true;

    }
  }
}

async function run() {
  const modulep = fetch(mathwasmUrl);
  const memory = new WebAssembly.Memory({ initial: 2 });
  MATH = await load(modulep, memory, { env: { memory } })

  THREE.OrbitControls = threeorbitcontrols(THREE);

  var canvas = document.getElementById('screen');
  var context = canvas.getContext('webgl2', { alpha: false });
  var renderer = new THREE.WebGLRenderer({ canvas, context });
  renderer.setSize(canvas.width, canvas.height);
  renderer.shadowMap.enabled = true;

  var scene = new THREE.Scene();
  var camera = new THREE.PerspectiveCamera(45,
    canvas.width / canvas.height,
    0.1, 1000);
  camera.position.set(100, 100, 100);
  camera.up = new THREE.Vector3(0, 0, 1);
  camera.lookAt(new THREE.Vector3(0, 0, 0));
  scene.add(camera);

  let spheres = add4Spheres(scene);
  s1 = spheres[0];
  s2 = spheres[1];
  s3 = spheres[2];
  ngeom = addNormal(scene, [0, 0, 0], [1, 1, 1]);

  var plane_geometry = new THREE.PlaneBufferGeometry(100, 100, 100, 100);
  var material = new THREE.MeshStandardMaterial({ color: 0xb7950b, side: THREE.DoubleSide, wireframe: false });
  var plane = new THREE.Mesh(plane_geometry, material);
  plane.castShadow = true;
  plane.receiveShadow = true;
  scene.add(plane);
  perlinGenerator(plane_geometry);
  plane_geometry.computeVertexNormals();
  let er = erosion(plane);

  var sgeometry = new THREE.SphereGeometry(0.5, 32, 32);
  var smaterial = new THREE.MeshBasicMaterial({ color: 0xff0000 });
  var sphere = new THREE.Mesh(sgeometry, smaterial);
  sphere.position.set(0.5, 0.5, 0);
  if (GLDEBUG) { scene.add(sphere); }

  document.addEventListener("keydown", e => {
    e.preventDefault()
    let step = 0.5;
    e = e || window.event;
    if (e.keyCode == '38') { sphere.position.setY(sphere.position.y - step); }
    else if (e.keyCode == '40') { sphere.position.setY(sphere.position.y + step); }
    else if (e.keyCode == '37') { sphere.position.setX(sphere.position.x + step); }
    else if (e.keyCode == '39') { sphere.position.setX(sphere.position.x - step); }

    setheightmapAt(plane, sphere, [0, 0, sgeometry.parameters.radius]);
  }, { passive: false });

  addOriginAxis(scene);

  let controls = new THREE.OrbitControls(camera);
  controls.enableDamping = true;
  controls.dampingFactor = 0.25;
  controls.enableZoom = true;
  controls.autoRotate = true;
  controls.enableKeys = false;


  var hemiLight = new THREE.HemisphereLight(0xffffff, 0xffffff, 0.6);
  console.log(hemiLight);
  hemiLight.color.setHSL(0.6, 0.75, 0.5);
  hemiLight.groundColor.setHSL(0.095, 0.5, 0.5);
  hemiLight.position.set(0, 500, 0);
  scene.add(hemiLight);
  var dirLight = new THREE.DirectionalLight(0xffffff, 1);
  dirLight.position.set(-1, 0.75, 1);
  dirLight.position.multiplyScalar(50);
  dirLight.name = "dirlight";
  dirLight.shadow.bias = -0.0001;
  dirLight.shadow.mapSize.width = 1024 * 4;
  dirLight.shadow.mapSize.height = 1024 * 4;
  scene.add(dirLight);
  let directionalLightHelper = new THREE.DirectionalLightHelper(dirLight, 50);
  scene.add(directionalLightHelper);

  function animate() {
    requestAnimationFrame(animate);

    for (var i = 0; i < 10000; ++i)
      er.step();
    plane_geometry.computeVertexNormals();
    renderer.render(scene, camera);

    ngeom.verticesNeedUpdate = false;
  }
  animate();
}

run();
