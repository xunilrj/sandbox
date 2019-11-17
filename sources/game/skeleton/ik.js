import './once.js';
import mouseCanvas  from './mouseCanvas.js';
import { Matrix, pseudoInverse } from 'ml-matrix'
function crossProduct(a, b, out)
{
    out = out || [];
    var ax = a[0], ay = a[1], az = a[2],
        bx = b[0], by = b[1], bz = b[2];
    out[0] = ay * bz - az * by;
    out[1] = az * bx - ax * bz;
    out[2] = ax * by - ay * bx;
    return out;
}

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
    var startAngle = getRotate();

    ctx.fillStyle = "lightblue";
    ctx.strokeStyle = "blue";
    ctx.beginPath();
        ctx.moveTo(x, y);
        ctx.arc(x, y, r, -(startAngle-angle), -(startAngle), true);
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
    let eff = bones.filter(x => x.isEffector)[0].effector_world;
    bones.forEach((x,i) => {
        var w = x.world;
        pushRotate(x.world.angle + x.angle);
            drawAngle(w.x, w.y, 10, x.angle);
            drawCircle(w.x, w.y, 20);
            drawCross(w.x, w.y, 20);
            drawBone(x, 20);
        popRotate();

        if(x.drawCircle)
        {
            var xl = eff.x - x.world.x;
            var yl = eff.y - x.world.y;
            var l = Math.sqrt(xl**2+yl**2);

            var w = x.world;
            let [cx,cy] = toCanvas(w.x,w.y);
            ctx.setLineDash([5, 5]);
            ctx.beginPath();                
                ctx.arc(cx, cy, l, 0, 2*Math.PI, true);            
            ctx.stroke();
            ctx.setLineDash([]);

            if(j)
            {
                eff = bones.filter(x => x.isEffector)[0];
                let [cx,cy] = toCanvas(
                    eff.effector_world.x,
                    eff.effector_world.y);


                var l = Math.sqrt(
                    (j.get(0, i)**2) +
                    (j.get(1, i)**2)
                );                
                var oldStrokeStyle = ctx.strokeStyle;
                ctx.strokeStyle = "red";
                ctx.beginPath();                
                    ctx.moveTo(cx,  cy);
                    ctx.lineTo(
                        cx + (j.get(0, i)/l)*100,  
                        cy + (j.get(1, i)/l)*100
                    );
                ctx.stroke();
                ctx.strokeStyle = oldStrokeStyle;
            }
        }
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

var j;
var jpath = [];
function updateJ()
{
    let recordPath = false;
    if(jpath.length == 0) recordPath = true;
    if(recordPath) jpath = [];

    let eff = bones.filter(x => x.isEffector)[0];
    //how much theta change, change effector pos?

    let steps = 10;
    var deffx = target[0] - eff.effector_world.x;
    var deffy = target[1] - eff.effector_world.y;
    var distance = Math.sqrt(
        deffx**2 +
        deffy**2
    );
    while(steps > 0 && Math.abs(distance) > 5) 
    {
        steps--;
        // 2xbones
        j = Matrix.zeros(2, bones.length);
        for(var i = bones.length - 1; i >= 0; --i)
        {        
            var b = bones[i];

            var dx = eff.effector_world.x - b.world.x;
            var dy = eff.effector_world.y - b.world.y;
            var cp = crossProduct([0,0,1],[dx,dy,0]);

            j.set(0, i, -cp[0]);
            j.set(1, i, -cp[1]);
        }

        // bonesx2
        let jPlus = pseudoInverse(j);

        var deffx = target[0] - eff.effector_world.x;
        var deffy = target[1] - eff.effector_world.y;
        //bonesx2 * 2x1 = bonesx1
        var dtheta = jPlus.mmul(new Matrix([[deffx],[deffy]]));

        var bonespos = [];
        for(var i = 0; i < bones.length; ++i)
        { 
            var b = bones[i];
            var a = dtheta.get(i, 0) * 0.1;
            var max = 0.01;
            if(a > max) a = max;
            if(a < -max) a = -max;
            b.angle += a;
            bonespos.push(b.angle);
        }
        jpath.push(bonespos);

        updateBones(skeleton, bones, {x:0,y:0,angle:0});

        deffx = target[0] - eff.effector_world.x;
        deffy = target[1] - eff.effector_world.y;
        distance = Math.sqrt(deffx**2 + deffy**2);

        
    } 

    // document.getElementById("stepDebug").innerText =
    //     JSON.stringify({bones,j, angles});
}

let ikEnabled = false;
document.getElementById("ikstep")
.addEventListener("click", e => {
    jpath = [];
    ikEnabled = e.target.checked;
});

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

    var [cx,cy] = toCanvas(0,0);
    var [ux,uy] = toCanvas(100,100);
    ctx.beginPath();
        ctx.moveTo(cx,cy);
        ctx.lineTo(ux,cy);
    ctx.stroke();
    ctx.beginPath();
        ctx.moveTo(cx,cy);
        ctx.lineTo(cx,uy);
    ctx.stroke();

    updateBones(skeleton, bones, {x:0,y:0,angle:0});
    if(ikEnabled)
        updateJ();
    drawBones(bones);
    
    drawPathTarget(bones, target);
    drawTarget(target);

    //draw state space    
    ctx.beginPath();
        ctx.rect(580, 20, 200, 200);
    ctx.stroke();
    function toAngleSpace(t1)
    {
        let x = t1 / (2*Math.PI);
        if(x < 0) x += 1;
        if(x > 1) x -= 1;
        return x;
    }
    ctx.beginPath();
        ctx.arc(
            580 + (toAngleSpace(bones[0].angle) * 200), 
             20 + (toAngleSpace(bones[1].angle) * 200),
            5, 0, 2*Math.PI, true);
    ctx.stroke();
    jpath.forEach(x => {
        ctx.beginPath();
            ctx.arc(
                580 + (toAngleSpace(x[0]) * 200), 
                20 + (toAngleSpace(x[1]) * 200),
                5, 0, 2*Math.PI, true);
        ctx.stroke();
    });
    ctx.fillText("θ2",565,215);
    ctx.fillText("θ1",580,235);

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
    root.addEventListener("mouseenter", x => {
        bones[i].drawCircle = true;
    });
    root.addEventListener("mouseleave", x => {
        bones[i].drawCircle = false;
    });
    root.append(input);
    root.append(label);

    document.getElementById("angles").append(root);
})



