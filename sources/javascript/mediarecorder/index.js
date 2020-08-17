// Import stylesheets
import './style.css';

// Write Javascript code!
const appDiv = document.getElementById('app');
appDiv.innerHTML = `<h1>JS Starter</h1>`;

function sleep(ms) {
    return new Promise(r => setTimeout(r, ms));
}

async function start() {
    let constraints = { video: true };
    let stream = await navigator.mediaDevices.getUserMedia(constraints);

    let cam = document.getElementById("cam");
    cam.srcObject = stream;
    cam.onloadedmetadata = function (e) {
        cam.play();
    };

    let chunks = [];
    var options = {
        audioBitsPerSecond: 128000,
        videoBitsPerSecond: 2500000,
        mimeType: 'video/webm; codecs=vp9'
    }
    var mediaRecorder = new MediaRecorder(stream, options);
    document.getElementById("record-start")
        .addEventListener("click", e => {
            chunks = [];
            mediaRecorder.start(16);
        });
    document.getElementById("record-end")
        .addEventListener("click", e => {
            mediaRecorder.stop();
            console.log(chunks);
        });

    mediaRecorder.ondataavailable = function (e) {
        chunks.push(e.data);
    }

    document.getElementById("record-play")
        .addEventListener("click", async e => {
            let vid = document.getElementById("vid");
            //var superBuffer = new Blob(chunks);    

            var source = new MediaSource();
            vid.src = URL.createObjectURL(source);
            vid.play();

            await sleep(1000);

            var sourceBuffer = source.addSourceBuffer('video/webm; codecs=vp9');
            sourceBuffer.mode = "sequence";
            source.addEventListener('sourceopen', e => {
                console.log(e);
            });

            let appendNext = async () => {
                let chunk = chunks.shift();
                if (!chunk) return;
                sourceBuffer.appendBuffer(await chunk.arrayBuffer())
            }
            sourceBuffer.addEventListener('updateend', appendNext);
            appendNext();
        });
}

start();