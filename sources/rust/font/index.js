import * as THREE from 'three';

let screen = document.getElementById('screen');
let context = screen.getContext('webgl2', { alpha: false });
let renderer = new THREE.WebGLRenderer({ canvas: screen, context: context });
//renderer.setPixelRatio(window.devicePixelRatio);
renderer.setSize(screen.width, screen.height);

let camera = new THREE.PerspectiveCamera(60, window.innerWidth / window.innerHeight, 0.1, 100);
camera.position.set(10, 10, 10);
camera.lookAt(0, 0, 0);

let scene = new THREE.Scene();

// let stats = new THREE.Stats();
// document.body.appendChild(stats.dom);

let geometry = new THREE.PlaneGeometry(1, 1, 1, 1);
let material = new THREE.MeshBasicMaterial({ color: 0xffff00, side: THREE.DoubleSide });
let mesh = new THREE.InstancedMesh(geometry, material, 1);
mesh.instanceMatrix.setUsage(THREE.DynamicDrawUsage); // will be updated every frame
scene.add(mesh);

function animate() {
    requestAnimationFrame(animate);
    renderer.render(scene, camera);
}
animate();