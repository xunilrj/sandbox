import * as tf from '/web_modules/@tensorflow/tfjs.js';

function sleep(ms) {
    return new Promise(r => setTimeout(r, ms));
}

async function loadTfModel() {
    return await tf.loadLayersModel("digits.json");
}
let tfModel = loadTfModel();

const indexOfMaxValue = (a, t = i => i) =>
    a.reduce((iMax, x, i, arr) => (t(x) > t(arr[iMax]) ? i : iMax), 0);
const printSudoku = (prediction, TOTAL_CELLS) => {
    let el = document.getElementById("sudoko-textarea");
    let result = "";
    for (let i = 0; i < TOTAL_CELLS; i++) {
        // obtain the 11 predicted states of this cell
        const cellPrediction = Array.from(prediction).slice(i * 11, i * 11 + 11);
        // what is the most likely digit for this cell?
        const digit = indexOfMaxValue(cellPrediction);
        result += digit < 10 ? digit : ".";

        // if (i % 3 == 2) {
        //     result += "   "
        // }
        // if (i % 9 == 8) {
        //     result += "\n"
        // }
        // if (i % 27 == 26) {
        //     result += "\n"
        // }
    }
    el.innerText = result;
    return result;
}

const solve = (puzzle) => {
    let el = document.getElementById("sudoko-textarea");
    el.innerText = puzzle.replace(/\./g, '0');
    return puzzle.replace(/\./g, '0');
}

const square = (a, b) => [a, b, a, a, b, a, b, b];
const applyPerspective = (src, srcCoords, dstCoords, w, h) => {
    const dst = cv.Mat.zeros(w, h, cv.CV_8UC3);
    const srcVertices = cv.matFromArray(4, 1, cv.CV_32FC2, srcCoords);
    const dstVertices = cv.matFromArray(4, 1, cv.CV_32FC2, dstCoords);
    const perspectiveTransform = cv.getPerspectiveTransform(srcVertices, dstVertices);

    cv.warpPerspective(
        src,
        dst,
        perspectiveTransform,
        new cv.Size(w, h),
        cv.INTER_LINEAR,
        cv.BORDER_CONSTANT,
        new cv.Scalar()
    );


    return dst;
}

const toGray = (from, to) => {
    if (from instanceof HTMLVideoElement) {
        let ctx = to.getContext("2d");
        ctx.drawImage(from, 0, 0, from.width, from.height);
        from = to;
    }

    let src = cv.imread(from);
    let originalWidth = src.cols;
    let originalHeight = src.rows;

    //toGray
    cv.cvtColor(src, src, cv.COLOR_RGBA2GRAY);
    cv.imshow("step1", src);

    //adaptiveThreshold
    cv.adaptiveThreshold(src, src, 255,
        cv.ADAPTIVE_THRESH_GAUSSIAN_C,
        cv.THRESH_BINARY,
        11,
        10);
    cv.imshow("step2", src);

    //findContours
    const contours = new cv.MatVector();
    const hierarchy = new cv.Mat();
    cv.findContours(
        src,
        contours,
        hierarchy,
        cv.RETR_CCOMP,
        cv.CHAIN_APPROX_SIMPLE
    );



    //findContours
    const color = index =>
        new cv.Scalar(
            (Math.sin(index) + 1.5) * 100,
            (Math.cos(index) + 1.5) * 100,
            0
        );
    const arrayRotate = (arr, count) => {
        count -= arr.length * Math.floor(count / arr.length);
        arr.push.apply(arr, arr.splice(0, count));
        return arr;
    };

    const rotateTopRightFirst = coords => {
        const pairs = [];
        for (let i = 0; i < 4; i++) {
            pairs[i] = [coords[i * 2], coords[i * 2 + 1]];
        }
        const bottomRight = indexOfMaxValue(pairs, p =>
            Math.sqrt(p[0] * p[0] + p[1] * p[1])
        );
        if (bottomRight !== 1) {
            const shift = -(1 - bottomRight);
            const newCoords = arrayRotate(coords, shift * 2);
            return newCoords;
        } else {
            return coords;
        }
    };
    const countourImageBuffer = cv.Mat.zeros(src.rows, src.cols, cv.CV_8UC3);
    const EPSILON = 10;
    const rectangles = [];
    for (let i = 0; i < contours.size(); ++i) {
        const contour = contours.get(i);
        const approximatedContour = new cv.Mat();

        cv.approxPolyDP(contour, approximatedContour, EPSILON, true);

        if (approximatedContour.size().height === 4) {
            rectangles.push({
                coord: Array.from(approximatedContour.data32S),
                area: cv.contourArea(approximatedContour)
            });
        }

        cv.drawContours(
            countourImageBuffer,
            contours,
            i,
            color(approximatedContour.size().height),
            1,
            cv.LINE_8,
            hierarchy,
            0
        );
        approximatedContour.delete();
    }
    contours.delete();
    hierarchy.delete();
    const idx = indexOfMaxValue(rectangles, r => r.area);
    let sudokuGrid = {
        coord: rotateTopRightFirst(rectangles[idx].coord),
        countourBuffer: countourImageBuffer
    };
    cv.imshow("step3", countourImageBuffer);

    //remove perspective
    let dst = applyPerspective(src,
        sudokuGrid.coord,
        square(180, 0),
        180,
        180);
    cv.imshow("step4", dst);

    //remove gridlines
    const GRID_STROKE = 4;
    let buffer = dst;
    const width = buffer.cols;
    const white = new cv.Scalar(255, 255, 255, 255);
    for (let i = 0; i <= 9; i++) {
        const pos = Math.floor((i * (width - GRID_STROKE)) / 9);
        let roi = buffer.roi(new cv.Rect(pos, 0, GRID_STROKE, width));
        roi.setTo(white);
        roi.delete();
        roi = buffer.roi(new cv.Rect(0, pos, width, GRID_STROKE));
        roi.setTo(white);
        roi.delete();
    }
    cv.imshow("step5", buffer);

    //digit recognition
    src = buffer;
    const IMAGE_WIDTH = 180;
    const getImageData = (src, x, y, width) => {
        const buffer = new Float32Array(width * width);
        let j = 0;
        for (let iy = y; iy < y + width; iy++) {
            for (let ix = x; ix < x + width; ix++) {
                buffer[j++] = src.data[ix + iy * IMAGE_WIDTH] / 255;
            }
        }
        return buffer;
    };
    let cellWidth = src.cols / 9;
    const cellSize = cellWidth * cellWidth;
    const TOTAL_CELLS = 81;
    let testDataArray = new Float32Array(src.cols * src.rows);
    for (let i = 0; i < TOTAL_CELLS; i++) {
        const x = (i % 9) * cellWidth,
            y = Math.floor(i / 9) * cellWidth;
        let buffer = getImageData(src, x, y, cellWidth);
        testDataArray.set(buffer, i * cellSize);
    }
    const testTensor = tf.tensor2d(testDataArray, [TOTAL_CELLS, cellSize]);
    const reshaped = testTensor.reshape([TOTAL_CELLS, cellWidth, cellWidth, 1]);
    const prediction = tfModel.predict(reshaped).dataSync();

    // SOLUTION
    const puzzle = printSudoku(prediction, TOTAL_CELLS);
    const solution = solve(puzzle);

    // RENDER SOLUTION
    // const digitsBuffer = renderDigits(projectedBuffer, solution);
    cv.imshow("step6", src);
    let canvas = document.getElementById("step6");
    ctx = canvas.getContext("2d");
    ctx.clearRect(0, 0, canvas.width, canvas.height);

    ctx.fillStyle = "red";
    ctx.font = "16px sans-serif";

    cellWidth = canvas.width / 9;

    for (let i = 0; i < TOTAL_CELLS; i++) {
        const digit = solution[i];
        if (digit === "0") {
            const x = (i % 9) * cellWidth,
                y = Math.floor(i / 9) * cellWidth;
            ctx.fillText(digit, x + 5, y + 15);
        }
    }
    src = cv.imread("step6");

    // apply perspective
    dst = applyPerspective(src,
        square(180, -2),
        sudokuGrid.coord,

        originalWidth,
        originalHeight
    );
    cv.imshow("step7", dst);

    // merge original image
    const step7 = document.getElementById("step7");
    const step7ctx = step7.getContext("2d");
    const toctx = to.getContext("2d");
    const srcData = step7ctx.getImageData(0, 0, step7.width, step7.height);
    const dstData = toctx.getImageData(0, 0, to.width, to.height);
    const srcDataData = srcData.data;
    const dstDataData = dstData.data;
    for (let i = 0; i < srcDataData.length; i += 4) {
        if (srcDataData[i + 3] !== 0) {
            const a = srcDataData[i + 3] / 255;
            const b = (255 - srcDataData[i + 3]) / 255;
            dstDataData[i] = a * 255 + b * dstDataData[i];
            dstDataData[i + 1] = b * dstDataData[i + 1];
            dstDataData[i + 2] = b * dstDataData[i + 2];
        }
    }
    const step8 = document.getElementById("step8");
    const step8ctx = step8.getContext("2d");
    step8ctx.putImageData(dstData, 0, 0);

    //show contours
    var ctx = document.getElementById("step2").getContext("2d");
    for (const r of rectangles) {
        const minx = Math.min(r.coord[0], r.coord[2], r.coord[4], r.coord[6]);
        const maxx = Math.max(r.coord[0], r.coord[2], r.coord[4], r.coord[6]);
        const miny = Math.min(r.coord[1], r.coord[3], r.coord[5], r.coord[7]);
        const maxy = Math.max(r.coord[1], r.coord[3], r.coord[5], r.coord[7]);
        ctx.strokeStyle = "gray";
        ctx.beginPath();
        ctx.rect(minx, miny, maxx - minx, maxy - miny);
        ctx.stroke();
    }
    const minx = Math.min(sudokuGrid.coord[0], sudokuGrid.coord[2], sudokuGrid.coord[4], sudokuGrid.coord[6]);
    const maxx = Math.max(sudokuGrid.coord[0], sudokuGrid.coord[2], sudokuGrid.coord[4], sudokuGrid.coord[6]);
    const miny = Math.min(sudokuGrid.coord[1], sudokuGrid.coord[3], sudokuGrid.coord[5], sudokuGrid.coord[7]);
    const maxy = Math.max(sudokuGrid.coord[1], sudokuGrid.coord[3], sudokuGrid.coord[5], sudokuGrid.coord[7]);
    ctx.strokeStyle = "red";
    ctx.beginPath();
    ctx.rect(minx, miny, maxx - minx, maxy - miny);
    ctx.stroke();


    src.delete();
};

async function start() {
    window.cv = await window.cv;
    tfModel = await tfModel;

    let constraints = { video: true };
    let stream = await navigator.mediaDevices.getUserMedia(constraints);

    let cam = document.getElementById("cam");
    cam.srcObject = stream;
    cam.onloadedmetadata = function (e) {
        cam.play();
        let camcanvas = document.getElementById("camcanvas");
        let ctx = camcanvas.getContext("2d");

        setInterval(() => {
            toGray(cam, camcanvas);
        }, 300);
        toGray(cam, camcanvas);


        // var data = idata.data;
        // for (var i = 0; i < data.length; i += 4) {
        //     var r = data[i];
        //     var g = data[i + 1];
        //     var b = data[i + 2];
        //     var brightness = (3 * r + 4 * g + b) >>> 3;
        //     data[i] = brightness;
        //     data[i + 1] = brightness;
        //     data[i + 2] = brightness;
        // }
        // idata.data = data;
        // ctx.putImageData(idata, 0, 0);
    };
}

start();