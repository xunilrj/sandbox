import {InferenceSession, Tensor} from 'onnxjs';
import onnxmodel from './model.onnx';
import "babel-polyfill";


const session = new InferenceSession();
session.loadModel(onnxmodel).then(async () => {
    const input = [
        new Tensor(new Float32Array([1.0,2.0]), "float32", [1,2])
    ];
    
    const button = document.getElementById("calc");
    button.disabled = false;
    button.addEventListener("click", async x => {
        const r = await session.run(input);

        const outputTensor = r.values().next().value;
        document.getElementById("result").innerText = outputTensor.data;
    });
});

