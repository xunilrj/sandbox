import * as THREE from 'three';
import { GLTFLoader } from 'three/examples/jsm/loaders/GLTFLoader';
import OrbitControls from 'three-orbitcontrols';
import 'babel-polyfill';
import modelUrl from './ruel.gltf';

function findAnim(animations, name) {
    // console.log(animations);
    return animations.filter(x => x.name == name)[0];
}

function ruelAnimState(model, animations) {
    this.model = model;
    this.animations = animations;
    this.current = "idle";
    this.mixer = new THREE.AnimationMixer(model);
    this.mixer.addEventListener('finished', (e) => {
        if (e.action.then)
            e.action.then(e);
    });
    this.clip = this.mixer.clipAction(
        findAnim(animations, "idle")
    ).play();
    this.nextClip = null;
    this.fadeDur = 0;

    model.waiting = {};

    this.crossFade = (oldClip, newClip, fadeDur = 0.1) => {
        if (newClip) {
            if (oldClip && fadeDur)
                oldClip.fadeOut(fadeDur);

            newClip.reset();
            if (oldClip && fadeDur)
                newClip.fadeIn(fadeDur);
            newClip.play();

            this.clip = newClip;
        }
    }

    this.update = (dt) => {

        this.mixer.update(dt);
    };

    this.wait = (dur) => {
        return new Promise((ok, err) => {
            let clip = new THREE.AnimationClip("timer", dur, [])
            let action = this.mixer.clipAction(clip);
            action.loop = THREE.LoopOnce;
            action.play();
            action.then = x => {
                action.stop();
                ok();
            };
        });
    }

    this.play = (name, fadeIn) => {
        let okk, errr;
        let p = new Promise((ok, err) => {
            okk = ok;
            errr = err;

            let clip = findAnim(animations, name);
            let nextClip = this.mixer.clipAction(clip);
            nextClip.then = ok;
            this.crossFade(this.clip, nextClip, fadeIn);
        });
        p.cancel = okk;
        return p;
    }

    this.playOnce = (name, fadeIn, f) => {
        let clip = findAnim(animations, name);
        let p = new Promise((ok, err) => {
            let nextClip = this.mixer.clipAction(clip);
            nextClip.loop = THREE.LoopOnce;
            nextClip.then = ok;
            this.crossFade(this.clip, nextClip, fadeIn);
        });

        if (f) return f(clip);
        else return p;
    }

    this.idle = async () => {
        this.current = "idle";
        this.play("idle");
        this.send = (cmd) => {
            if (cmd.name == "start.walk") {
                this.walking();
            }
        }
    }

    this.walking = async () => {
        this.current = "walking";

        let p = this.play("walking", 0.1);
        this.send = (cmd) => {
            if (cmd.name == "stop.walk") {
                p.cancel();
            }
        }
        await p;
        let dur = await this.playOnce("walkingstop", 0.1, x => x.duration - 0.1);
        await this.wait(dur);
        this.idle();
    }

    this.idle();
}

let scene, camera, renderer, controls, mixer, gltf, animState;
let walking = 0;
function keyDownHandler(e) {
    if (e.key == "Up" || e.key == "ArrowUp") {
        animState.send({ name: "start.walk" });
    }
}
function keyUpHandler(e) {
    if (e.key == "Up" || e.key == "ArrowUp") {
        animState.send({ name: "stop.walk" });
    }
}
document.addEventListener("keydown", keyDownHandler, false);
document.addEventListener("keyup", keyUpHandler, false);


let lastTimestamp;
function animate(timestamp) {
    if (!lastTimestamp) { lastTimestamp = timestamp - 16; }

    let dt = (timestamp - lastTimestamp) / 1000;

    if (animState)
        animState.update(dt);

    renderer.render(scene, camera);

    lastTimestamp = timestamp;
    requestAnimationFrame(animate);
}

async function init() {
    renderer = new THREE.WebGLRenderer();
    renderer.setSize(800, 600);
    renderer.shadowMap.enabled = true;
    document.body.appendChild(renderer.domElement);

    scene = new THREE.Scene();
    scene.background = new THREE.Color(0xdddddd);

    camera = new THREE.PerspectiveCamera(60,
        800 / 600, 1, 5000);
    camera.position.set(3, 3, 3);

    controls = new OrbitControls(camera, renderer.domElement);
    scene.add(new THREE.AxesHelper(500));

    var geometry = new THREE.PlaneGeometry(5, 5, 20);
    var material = new THREE.MeshStandardMaterial({ color: 0xffff00, side: THREE.DoubleSide });
    var plane = new THREE.Mesh(geometry, material);
    plane.rotateX(3.14159 / 2.0);
    plane.receiveShadow = true;
    scene.add(plane);

    const hemiLight = new THREE.HemisphereLight(0xffeeb1, 0x080820, 1);
    // hemiLight.castShadow = true;
    scene.add(hemiLight);
    //var helper = new THREE.HemisphereLightHelper(hemiLight, 5);
    //scene.add(helper);

    const light = new THREE.SpotLight(0xffffff);
    light.position.set(3, 10, -3);
    light.castShadow = true;
    light.shadow.bias = -0.0001;
    light.shadow.mapSize.width = 1024;
    light.shadow.mapSize.height = 1024;
    scene.add(light);
    //var spotLightHelper = new THREE.SpotLightHelper(light);
    //scene.add(spotLightHelper);
    //const cameraHelper = new THREE.CameraHelper(light.shadow.camera);
    //scene.add(cameraHelper);

    new GLTFLoader().load(modelUrl,
        r => {
            gltf = r;
            // console.log(gltf);
            const model = gltf.scene.children[0];
            scene.add(model);
            animState = new ruelAnimState(model, gltf.animations);

            model.castShadow = true;
            model.receiveShadow = true;
            model.traverse(n => {
                if (n.isMesh) {
                    n.castShadow = true;
                    n.receiveShadow = true;
                }
            });
        });

    animate();
}
init();