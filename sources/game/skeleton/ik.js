import './once.js';
import mouseCanvas  from './mouseCanvas.js';

const canvas = document.querySelector("canvas");

//////////// ROTATION

let rotates = [];
function pushRotate(angle) { rotates.push(angle); }
function popRotate(angle) { rotates.pop(); }
function getRotate()
{
    if(rotates.length > 0) return rotates[rotates.length-1];
    else return 0;
}

function useRotation(x,y)
{
    var angle = getRotate();
    let px = x*Math.cos(angle) + y*Math.sin(angle);
    let py = x*-Math.sin(angle) + y*Math.cos(angle);
    return [px,py];
}



let target = [200,-200];

mouseCanvas(canvas)
    .toCanvas(toCanvas)    
    .addRect((h) => h.rectAround(target, 5))
    .state("select", {
        callback: e => {
            switch(e.type)
            {
                case "enter": { e.cursor = "grab"; break; }
                case "exit": { e.cursor = "default"; break; }
            }
        },
        mouseDown: e => {
            if(e.rect) e.state = "drag";
        }
    })
    .state("drag", {
        mouseMove:e => {
            e.cursor = "grabbing";
            const [x,y] = fromCanvas(e.x,e.y);
            target[0] = x;
            target[1] = y;
        },
        mouseUp: e => {
            e.state = "select";
            e.cursor = "grab";
        }
    });

const ctx = canvas.getContext("2d");




function toCanvas(x,y) { return [x+canvas.width/5, y+4*canvas.height/5]; }
function fromCanvas(x,y) { return [x-canvas.width/5, y-4*canvas.height/5]; }
function drawCircle(x,y,r)
{
    [x,y] = toCanvas(x,y);
    ctx.beginPath();
    ctx.arc(x, y, r, 0, 2 * Math.PI);
    ctx.stroke();
}
function drawCross(x,y,r)
{
    [x,y] = toCanvas(x,y);
    var [fx,fy] = useRotation(-r,0);
    var [tx,ty] = useRotation(+r,0);
    ctx.beginPath();
        ctx.moveTo(x+fx, y+fy);
        ctx.lineTo(x+tx, y+ty);
    ctx.stroke();

    [fx,fy] = useRotation(0,-r);
    [tx,ty] = useRotation(0,+r);
    ctx.beginPath();
        ctx.moveTo(x+fx, y+fy);
        ctx.lineTo(x+tx, y+ty);
    ctx.stroke();
}
function drawAngle(x,y,r,angle)
{
    [x,y] = toCanvas(x,y);

    ctx.fillStyle = "lightblue";
    ctx.strokeStyle = "blue";
    ctx.beginPath();
    ctx.moveTo(x, y);
    ctx.arc(x, y, r, 0, -angle, true);
    ctx.closePath();
    ctx.fill();
    var oldLineWidth = ctx.lineWidth;
    ctx.lineWidth = 1;
    ctx.stroke();
    ctx.lineWidth = oldLineWidth;

    ctx.fillStyle = "black";
    ctx.strokeStyle = "black";
}
function drawBone(x, r)
{
    var w = x.world;
    var ew = x.effector_world;
    let [cx,cy] = toCanvas(w.x,w.y);
    let [ex,ey] = toCanvas(ew.x,ew.y);
    let [tx,ty] = useRotation(0,-r);
    let [bx,by] = useRotation(0,+r);

    ctx.beginPath();
        ctx.moveTo(cx+tx, cy+ty);
        ctx.lineTo(ex, ey);    
        ctx.lineTo(cx+bx, cy+by);
    ctx.stroke();
}
function drawBones(bones)
{
    bones.forEach((x,i) => {
        var w = x.world;
        drawAngle(w.x, w.y, 10, x.angle);
        pushRotate(x.angle);
            drawCircle(w.x, w.y, 20);
            drawCross(w.x, w.y, 20);
            drawBone(x, 20);
        popRotate();
    });
}

function drawDashedLine(x1,y1,x2,y2)
{
    [x1,y1] = toCanvas(x1,y1);
    [x2,y2] = toCanvas(x2,y2);

    ctx.setLineDash([5, 5]);
    ctx.beginPath();
        ctx.moveTo(x1,y1);
        ctx.lineTo(x2,y2);
    ctx.stroke();
    ctx.setLineDash([])
}

function drawPathTarget(bones, target)
{
    let e = bones.filter(x => x.isEffector)[0];
    drawDashedLine(
        e.effector_world.x, e.effector_world.y,
        target[0], target[1]
    );
}

function drawTarget(target)
{
    drawCircle(target[0], target[1], 5);
    drawCross(target[0], target[1], 5);
}

let bones = [
    {length:200, angle:Math.PI/4, world:{}},
    {length:200, angle:Math.PI/4, world:{}, isEffector: true},
];
let skeleton = [
    {children:[1]}    
]

function updateBone(skeleton, bones, i, world)
{
    var bone = bones[i];
    bone.world.x = world.x;
    bone.world.y = world.y;
    bone.world.angle = world.angle;

    var t = world.angle + bone.angle;
    var l = bone.length;
    bone.effector_world = {};
    bone.effector_world.angle = t;
    bone.effector_world.x = world.x + (l*Math.cos(t) + 0*Math.sin(t));
    bone.effector_world.y = world.y + (l*-Math.sin(t) + 0*Math.cos(t));
    
    if(skeleton[i] && skeleton[i].children) {
        var children = skeleton[i].children;
        children.forEach(xx => {
            updateBone(skeleton, bones, xx, bone.effector_world);
        });
    }
}

function updateBones(skeleton, bones, world)
{
    skeleton.forEach((x,i) => 
        updateBone(skeleton, bones, i, world)
    );
}

function render() {
    ctx.clearRect(0,0,canvas.width,canvas.height);

    updateBones(skeleton, bones, {x:0,y:0,angle:0});
    drawBones(bones);
    
    drawPathTarget(bones, target);
    drawTarget(target);

    requestAnimationFrame(render);
}
requestAnimationFrame(render);

bones.forEach((x,i) => {
    let input = document.createElement("input");
    let label = document.createElement("label");
    input.style = "width: 400px";
    input.type = "range";
    input.min = 0;
    input.max = 360;
    input.addEventListener("input", x => {
        bones[i].angle = parseFloat(input.value / 180*Math.PI);
        label.innerText = input.value;
    });
    let root = document.createElement("div");
    root.append(input);
    root.append(label);

    document.getElementById("angles").append(root);
})



