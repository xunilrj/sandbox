// Import stylesheets
import './style.css';
import start from 'Game.js';
import AStar from 'AStar.js';
import WalkGraph from 'WalkGraph.js';
import buildIncidence from 'BuildIncidence.js';
import log from 'Console.js';
import { myclamp, vec, vecscal, vecdot, vecminus, vecLength, vecNormalize, vecDraw } from 'Math.js';
import GetNearestPoint from 'GetNearestPoint.js';
import Sprite from 'Sprite.js';
import guybrushConfig from 'GuybrushConfig.js';
import cannonConfig from 'CannonConfig.js';
import Graphics from 'Graphics.js';

var el = document.getElementById("console");
const log = (x) => {
  if (el.children.length >= 5) {
    el.removeChild(el.firstChild);
  }
  var ellog = document.createElement('pre');
  if (arguments.length == 1)
    ellog.innerHTML = JSON.stringify(x);
  else
    ellog.innerHTML = JSON.stringify(Array.from(arguments));
  el.appendChild(ellog);
}

function NearestPoint(config) {
  const Point = [10, 10];
  this.update = (dt, data) => {
    if (data.mouseClicks.length > 0) {
      
      Point = data.mouseClicks[0].pos;
    }
  };
  const getXY = (edge, t) => {
    if (t < 0) t = 0;
    if (t > 1) t = 1;
    var e = config.edges[edge];
    var n0 = config.nodes[e.nodes[0]];
    var n1 = config.nodes[e.nodes[1]];
    var v = [n1.pos[0] - n0.pos[0], n1.pos[1] - n0.pos[1]];
    return [
      v[0] * t + n0.pos[0],
      v[1] * t + n0.pos[1]];
  };
  this.render = (ctx, data) => {
    ctx.fillStyle = "Green";
    ctx.beginPath();
    ctx.arc(Point[0], Point[1], 4, 0, 2 * Math.PI);
    ctx.fill();

    var best = GetNearestPoint(Point, config, ctx);
    var nearest = getXY(best[0], best[4]);
    ctx.beginPath();
    ctx.arc(nearest[0], nearest[1], 2, 0, 2 * Math.PI);
    ctx.fill();
  };
}

const config = buildIncidence([
  { pos: [170, 300] },//0
  { pos: [270, 300] },//1
  { pos: [140, 330] },//2
  { pos: [270, 330] },//3
  { pos: [320, 350] },//4
  { pos: [440, 330] },//5
  { pos: [500, 370] },//6
  { pos: [520, 430] },//7
  { pos: [220, 430] },//8
  { pos: [300, 380] },//9
  { pos: [420, 430] },//10
  { pos: [320, 430] },//11
  { pos: [430, 360] },//12
  { pos: [420, 390] },//13
  { pos: [370, 400] },//14
], [
    { nodes: [0, 1] },
    { nodes: [0, 2] },
    { nodes: [1, 2] },
    { nodes: [1, 3] },
    { nodes: [2, 3] },
    { nodes: [3, 4], begin:{z:0.4}, end: {z:0.6} },
    { nodes: [4, 5] },
    { nodes: [5, 6] },
    { nodes: [6, 7] },
    { nodes: [7, 10] },
    { nodes: [10, 11] },
    { nodes: [11, 8] },
    { nodes: [8, 9] },
    { nodes: [9, 4] },
    { nodes: [4, 12] },
    { nodes: [12, 6] },
    { nodes: [4, 13] },
    { nodes: [13, 7] },
    { nodes: [13, 10] },
    { nodes: [12, 13] },
    { nodes: [12, 5] },
    { nodes: [12, 7] },
    { nodes: [4, 14] },
    { nodes: [14, 11] },
    { nodes: [14, 10] },
    { nodes: [14, 13] },
    { nodes: [4, 11] },
    { nodes: [9, 11] },
  ]);

const sprite = new Sprite(0, guybrushConfig, {
  animation: "standingDownLeft",
  pos: [0,0],
  z : 0.4
})
start({
  graphics: new Graphics(),
  init: x => {
    return {
      assets: [
        x.loadImage("http://localhost:8081/11704.png"),//0
        x.loadImage("http://localhost:8081/mi3/Room009/00.png"),//1
        x.loadImage("http://localhost:8081/mi3/Room009/01.png"),//2
        x.loadImage("http://localhost:8081/mi3/Room009/02.png"),//3
        x.loadImage("http://localhost:8081/mi3/cannonanim.png"),//4
        x.loadImage("http://localhost:8081/mi3/obj_279_1.png"),//5
        x.loadImage("http://localhost:8081/mi3/obj_105_1.png"),//6
        x.loadImage("http://localhost:8081/mi3/obj_105_2.png"),//7
      ],
      components: [
        new WalkGraph(config, x => {
          var pos = x.pos;
          sprite.data.pos = [pos[0]-40,pos[1]-250];

          var animationName = "standing";
          if(!x.dir) x.dir = "DownLeft";
          if(x.moving) animationName = "walking";
          sprite.data.animation = animationName + x.dir;

          if(x.info.z) sprite.data.z = x.info.z
        }),
        new NearestPoint(config),
        sprite,
        new Sprite(4, cannonConfig, {
          animation: "standing",
          pos: [5,217],
          z:0.5
        })
      ],
      cursor: {
        default: false,
        asset : 6,
        adjust: [-35,-25]
      }
    };
  },
  update: (dt, data) => {
    data.log(data.mouseClicks, data.mousePos);
    if (data.mouseClicks.length > 0) {
      var point = data.mouseClicks[0].pos
      var best = GetNearestPoint(point, config);
      data.components[0].walkTo([best[0], best[4]]);
    }
    if(data.mousePos[0]){
      data.cursor.asset = 6;
      if(data.mousePos[0].pos[0] > 216 &&
         data.mousePos[0].pos[0] < 400 &&
         data.mousePos[0].pos[1] > 184 &&
         data.mousePos[0].pos[1] < 216){
        data.cursor.asset = 7;
      }
    }
  },
  render: (ctx, data) => {
    ctx.clearRect(0, 0, 640, 480);
    ctx.drawImageZ(0, data.assets[1], 0, 0);
    ctx.drawImageZ(0, data.assets[5], 216, 184);
    ctx.drawImageZ(1, data.assets[2], 0, 0);    
    ctx.drawImageZ(2, data.assets[3], 0, 0);

    
  }
});