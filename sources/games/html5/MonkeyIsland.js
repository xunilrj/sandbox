import output from "Console.js"
import guybryshConfig from "./Guybrush.sprite.js";
import Sprite from "./Sprite.js";

function removeSmallest (array, f) {
  var newArray = [];
  var minitem;
  var minv
  for(var i =0; i < aray.length; ++i) {
    var current = array[i];
    if(!minitem) {
      minitem = current;
      minv = f(minitem);
    } else {
      var currentv = f(current);
      if(minv > currentv) {
        newArray.push(minitem);
        minitem = current;
        minv = currentv;        
      } else {
        newArray.push(current);
      }
    }
  }
  return newArray;    
};
function myclamp(x, min, max) {
  if (x < min) return min;
  if (x > max) return max;
  return x;
}
function vecscal(x, s) {
  return [x[0] * s, x[1] * s];
}
function vecdot(x, y) {
  return x[0] * y[0] + x[1] * y[1];
}
function vecminus(x, y) {
  return [x[0] - y[0], x[1] - y[1]];
}
function vecLength(x) {
  return Math.sqrt(x[0] * x[0] * 1.0 + x[1] * x[1] * 1.0);
}
function vecNormalize(v) {
  var l = vecLength(v) * 1.0;
  return [v[0] * 1.0 / l, v[1] * 1.0 / l];
}
function vec(from, to, normalize) {
  if (!normalize)
    return [to[0] - from[0], to[1] - from[1]];
  else {
    return vecNormalize([to[0] - from[0], to[1] - from[1]]);
  }
}
function vecDraw(ctx, pos, v, color) {
  color = color || "green"
  ctx.strokeStyle = color;
  ctx.beginPath();
  ctx.moveTo(pos[0], pos[1]);
  ctx.lineTo(pos[0] + v[0], pos[1] + v[1]);
  ctx.stroke();
}

const canvas = document.getElementById('canvas');
const ctx = canvas.getContext("2d");

function loadImage(url) {
  return new Promise(resolve => {
    let i = new Image();
    i.onload = () => { resolve(i) };
    i.src = url;
  });
}


let assets = {
  ImageGuybrush: loadImage("http://localhost:8081/11704.png"),
  ImageBackgroungCannon: loadImage("http://localhost:8081/mi3/cannon.png"),
  ImageDoor: loadImage("http://localhost:8081/mi3/obj_275_1.png"),
  ImageCannonFodder: loadImage("http://localhost:8081/mi3/obj_279_1.png"),
  ImageCannon: loadImage("http://localhost:8081/mi3/cannonanim.png"),
};
const data = Object.create(assets);
data.draw = "sprite";
data.tx = 0;
data.ty = 0;
data.drawDoor = true;
data.drawCannonFodder = true;
data.drawCannon = true;
data.Pause = false;
data.Sprites = {
  Guybrush: guybryshConfig,
  Cannon: cannonConfig
};

function Nodes(setf, endf) {
  this.data = {
    dt: 0,
    path: [],
    pos: [0, 0, 0],
    dirCode: "",
    target: [340, 350],
    nodes: [
      [450, 440],
      [340, 350],
      [250, 450]
    ],
    edges: [
      { nodes: [0, 1], vel: 1 },
      { nodes: [1, 2], vel: 1 }
    ],
    mousePos: null
  };
  this.pushPath = (newNode) => {
    if (this.data.pos[0] == newNode[0]) {
      if (newNode[1] > this.data.pos[2]) {
        this.data.path.push([newNode[0], 0, newNode[1]]);
      } else {
        this.data.path.push([newNode[0], 1, newNode[1]]);
      }
    } else {
      var path = this.astar([this.data.pos[0]], newNode);
      var lastPos;
      for (var i = 0; i < path.length; ++i) {
        var current = path[i];
        if (this.data.pos[0] == current[0]) {
          if (current[1] > this.data.pos[2]) {
            this.data.path.push([current[0], 0, current[1]]);
            lastPos = 1;
          } else {
            this.data.path.push([current[0], 1, current[1]]);
            lastPos = 0;
          }
        } else {
          var currentPos = lastPos == 0 ? 1 : 0;          
          this.data.path.push([current[0], currentPos, current[1]]);
          lastPos = currentPos;
        }
      }  
    }
  };
  this.updatePos = (dt) => {
    if (this.data.path.length == 0) { return; }

    var current = this.data.path[0];
    var edge = this.data.edges[current[0]];
    if (!this.data.pos) {
      this.data.pos = [current[0], current[1], 0, 0, 0, ""]
    }

    var f = current[1] == 0 ? 1 : -1;
    var t = this.data.pos[2] + edge.vel * dt * f;
    if (t >= current[2] && current[1] == 0) { t = current[2]; }
    if (t <= current[2] && current[1] == 1) { t = current[2]; }

    var nodeFrom = this.data.nodes[edge.nodes[0]];
    var nodeTo = this.data.nodes[edge.nodes[1]];
    var dir = [nodeTo[0] - nodeFrom[0], nodeTo[1] - nodeFrom[1]];
    var x = dir[0] * t + nodeFrom[0];
    var y = dir[1] * t + nodeFrom[1];

    if (current[1] == 1) dir = vecscal(dir, -1);
    var dirCode;
    if (dir[1] > +0.1) dirCode = "Down";
    else if (dir[1] < -0.1) dirCode = "Up";
    if (dir[0] > +0.1) dirCode += "Right";
    else if (dir[0] < -0.1) dirCode += "Left";

    this.data.pos = [current[0], current[1], t, x, y, dirCode];
  };
  this.update = function (dt) {
    if (this.data.path.length > 0) {
      this.updatePos(dt);
      if (setf) {
        setf(this.data.pos[3], this.data.pos[4], this.data.pos[5]);
      }

      var current = this.data.path[0];
      if (this.data.pos && this.data.pos[1] == 0) {
        if (this.data.pos[2] >= current[2]) {
          this.data.path.shift();
          if (this.data.path.length == 0) endf(this.data.pos[5]);
          else if (this.data.pos[2] >= 1.0) this.data.pos[2] = 0;
        }
      } else if (this.data.pos && this.data.pos[1] == 1) {
        if (this.data.pos[2] <= current[2]) {
          this.data.path.shift();
          if (this.data.path.length == 0) endf(this.data.pos[5]);
          else if (this.data.pos[2] <= 0.0) this.data.pos[2] = 1;
        }
      }
    }
  };

  this.astar = (from, to) => {    
    if ((from[0] == 0) && (to[0] == 1)) {
      return [[0, 1], [1, to[1]]];
    } else if ((from[0] == 1) && (to[0] == 0)) {
      return [[1, 0], [0, to[1]]];
    }

    //push startNode onto openList
    var openList = [from];
    var closedList = []; 
    // while(openList is not empty) {
    while(openList.length > 0) {
      //currentNode = find lowest f in openList
      var currentNode = removeSmallest(openList, x => 0);
      //if currentNode is final, return the successful path
      if(currentNode[0] == to[0]){
        return successPath;
      }
      //push currentNode onto closedList and remove from openList
      closedList.push(currentNode[0]);
      //foreach neighbor of currentNode {
      var neighbors = getNeighbors(currentNode);
      neighbors.forEach(neighbor => {
        //if neighbor is not in openList {
        var indexOf = openList.indexOf(x => x[0] == neighbor[0]);
        if(indexOf < 0){
          //save g, h, and f then save the current parent
          //add neighbor to openList
          openlist.push(neighbor);
        //}  
        //if neighbor is in openList but the current g is better than previous g {
        } else {
            //save g and f, then save the current parent
        }
      });
    }
  };
  this.render = function (ctx) {
    // Nodes and Edges
    ctx.lineWidth = "1";
    ctx.strokeStyle = "red";
    for (var i = 0; i < this.data.edges.length; ++i) {
      var current = this.data.edges[i];
      var nodes = current.nodes;
      var node1 = this.data.nodes[nodes[0]];
      var node2 = this.data.nodes[nodes[1]];
      ctx.beginPath();
      ctx.moveTo(node1[0], node1[1]);
      ctx.lineTo(node2[0], node2[1]);
      ctx.stroke();
    }
    for (var i = 0; i < this.data.nodes.length; ++i) {
      var current = this.data.nodes[i];
      ctx.fillStyle = "#FF0000FF";
      ctx.beginPath();
      ctx.arc(current[0], current[1], 2, 0, 2 * Math.PI);
      ctx.fill();
      ctx.font = "bold 16px Arial";
      ctx.fillText(i.toString(), current[0], current[1]);
    }

    //Current Position    
    ctx.fillStyle = "#0000FFFF";
    ctx.beginPath();
    ctx.arc(this.data.pos[3], this.data.pos[4], 5, 0, 2 * Math.PI);
    ctx.fill();

    if (this.data.mousePos) {
      ctx.fillStyle = "#00FF00FF";
      ctx.beginPath();
      ctx.arc(this.data.mousePos[0], this.data.mousePos[1], 2, 0, 2 * Math.PI);
      ctx.fill();

      var best = null;
      for (var i = 0; i < this.data.edges.length; ++i) {
        var current = this.data.edges[i];
        var nodes = current.nodes;
        var node1 = this.data.nodes[nodes[0]];
        var node2 = this.data.nodes[nodes[1]];

        var vece = vec(node1, node2, true);
        var vec1 = vec(node1, this.data.mousePos);
        var vec2 = vec(node2, this.data.mousePos);

        vecDraw(ctx, node1, vece);
        vecDraw(ctx, node1, vec1);
        vecDraw(ctx, node2, vec2);

        var along = vecdot(vec1, vece);
        var projVec1 = vecscal(vece, along);
        vecDraw(ctx, node1, projVec1);
        var vecDist = vecminus(projVec1, vec1);
        var dist = vecLength(vecDist);
        vecDraw(ctx, this.data.mousePos, vecDist);

        if (!best) {
          best = [i, 0, along, dist];
        } else {
          if (best[3] > dist) {
            best = [i, 0, along, dist];
          }
        }
      }

      var current = this.data.edges[best[0]];
      var nodes = current.nodes;
      var node1 = this.data.nodes[nodes[0]];
      var node2 = this.data.nodes[nodes[1]];
      var v = vecscal(vecNormalize(vecminus(node2, node1)), best[2]);
      vecDraw(ctx, node1, v, "grey");
    }
  },
    this.getNearest = (point) => {
      var best = null;
      for (var i = 0; i < this.data.edges.length; ++i) {
        var current = this.data.edges[i];
        var nodes = current.nodes;
        var node1 = this.data.nodes[nodes[0]];
        var node2 = this.data.nodes[nodes[1]];

        var vece = vec(node1, node2, true);
        var vec1 = vec(node1, point);
        var vec2 = vec(node2, point);

        var along = vecdot(vec1, vece);
        var normalizedAlong = myclamp(along / vecLength(vecminus(node1, node2)), 0.0, 1.0);
        var projVec1 = vecscal(vece, along);
        var vecDist = vecminus(projVec1, vec1);
        var dist = vecLength(vecDist);

        if (!best) {
          best = [i, 0, along, dist, normalizedAlong];
        } else {
          if (best[3] > dist) {
            best = [i, 0, along, dist, normalizedAlong];
          }
        }
      }
      return best;
    }
}

data.Objects = [
  [0, "Door", "ImageDoor", 432, 112],
  [0, "CannonFodder", "ImageCannonFodder", 216, 184],
  [1, null, "Guybrush", "standingDownLeft", 400, 180],
  [1, null, "Cannon", "standing", 5, 217],
  [2, new Nodes((x, y, dirCode) => {
    data.Objects[2][1].data.pos[0] = x - 50;
    data.Objects[2][1].data.pos[1] = y - 250;
    data.Objects[2][1].data.animation = "walking" + dirCode
  }, (dirCode) => {
    data.Objects[2][1].data.animation = "standing" + dirCode
  })],
];



document.getElementById("pause").addEventListener("click", function () {
  data.Pause = !data.Pause;
});
document.getElementById("animation").addEventListener("change", function (e) {
  data.Guybrush.data.animation = e.target.value;
});
function getCursorPosition(canvas, event) {
  var rect = canvas.getBoundingClientRect();
  var x = event.clientX - rect.left;
  var y = event.clientY - rect.top;
  return [x, y];
}
document.addEventListener("click", function (e) {
  if (e.target == canvas) {
    var p = getCursorPosition(canvas, e);
    var nearest = data.Objects[4][1].getNearest(p);
    data.Objects[4][1].data.mousePos = p;
    data.Objects[4][1].pushPath([nearest[0], nearest[4]]);
  }
});
document.addEventListener("change", function (e) {
  if (e.target.type == "checkbox") {
    console.log("change", e.target.id, e.target.checked);
    data[e.target.id] = e.target.checked;
  } else {
    console.log("change", e.target.id, e.target.value);
    data[e.target.id] = parseFloat(e.target.value);
  }
});
data.keyPressed = {};
document.addEventListener("keydown", function (e) {
  data.keyPressed[e.keyCode] = true;
});
document.addEventListener("keyup", function (e) {
  data.keyPressed[e.keyCode] = false;
});
Promise.all(Object.keys(assets).map(x => assets[x])).then(result => {
  let i = 0;
  Object.keys(assets).forEach(x => {
    assets[x] = result[i++];
  });

  for (var i = 0; i < data.Objects.length; ++i) {
    var current = data.Objects[i];
    if (current[0] == 1) {
      current[1] = new Sprite(data["Image" + current[2]], data.Sprites[current[2]], {
        animation: current[3],
        pos: [current[4], current[5]]
      });
    }
  }

  let lastTimestamp = null;
  function render(timestamp) {
    if (!lastTimestamp) lastTimestamp = timestamp - 16;
    const dt = (timestamp - lastTimestamp) / 1000.0;
    lastTimestamp = timestamp;



    //ctx.setTransform(1, 0, 0, 1, 0, 0);
    ctx.clearRect(0, 0, canvas.width, canvas.height);


    if (data.draw == "sprites") {
      var spriteName = document.getElementById("sprite").value;

      ctx.save()
      ctx.translate(data.tx, data.ty);
      if (data["Image" + spriteName]) {
        ctx.drawImage(data["Image" + spriteName], 0, 0);
      }

      var sprite = data.Sprites[spriteName];
      if (sprite) {
        var keys = Object.keys(sprite.animations);
        keys.forEach(function (anim) {
          var animation = sprite.animations[anim];
          if (animation.include) {
            return;
          }
          var frames = animation.frames;
          for (var i = 0; i < frames.length; ++i) {
            var current = frames[i].rect;
            ctx.beginPath();
            ctx.lineWidth = "1";
            if ((i % 2) == 0) {
              ctx.strokeStyle = "red";
            } else {
              ctx.strokeStyle = "green";
            }
            ctx.rect(current[0], current[1], current[2], current[3]);
            ctx.stroke();
          }
        });

        ctx.restore();
      }

    } else {

      var obj = data.Objects[2][1].data;
      if (data.keyPressed[39] && data.keyPressed[40])
        obj.animation = "walkingDownRight";
      else if (data.keyPressed[37] && data.keyPressed[40])
        obj.animation = "walkingDownLeft";
      else if (data.keyPressed[39] && data.keyPressed[38])
        obj.animation = "walkingUpRight";
      else if (data.keyPressed[37] && data.keyPressed[38])
        obj.animation = "walkingUpLeft";
      else if (data.keyPressed[38])
        obj.animation = "walkingUp";
      else if (data.keyPressed[40])
        obj.animation = "walkingDown";
      else if (data.keyPressed[39])
        obj.animation = "walkingRight";
      else if (data.keyPressed[37])
        obj.animation = "walkingLeft";

      if (!data.Pause) {
        for (var i = 0; i < data.Objects.length; ++i) {
          var current = data.Objects[i];
          if (current[0] == 1) {
            current[1].update(dt);
          } else if (current[0] == 2) {
            current[1].update(dt);
          }
        }
      }

      ctx.drawImage(data.ImageBackgroungCannon, 0, 0);

      for (var i = 0; i < data.Objects.length; ++i) {
        var current = data.Objects[i];
        if (data["draw" + current[1]]) {
          ctx.drawImage(data[current[2]], current[3], current[4]);
        }
        if (current[0] == 1) {

        }
      }

      for (var i = 0; i < data.Objects.length; ++i) {
        var current = data.Objects[i];
        if (current[0] == 1) {
          current[1].render(ctx);
        } else if (current[0] == 2) {
          current[1].render(ctx);
        }
      }
    }
    requestAnimationFrame(render);
  }
  requestAnimationFrame(render);
});
