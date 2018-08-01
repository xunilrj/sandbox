import {myclamp, vec, vecscal, vecdot, vecminus, vecLength, vecNormalize, vecDraw} from 'Math.js';

export default function Nodes(setf, endf) {
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
      { nodes: [0, 1], vel: 1, end: [[1, 0]] },
      { nodes: [1, 2], vel: 1, begin: [[0, 1]] }
    ],
    mousePos: null
  };
  this.astar = (from, to) => {
    var getNeighbors = (x) => {
      var edgeNum = x.node[0];
      var edge = this.data.edges[0];
      var neighbors = [];
      if (x.node[1] != 0) neighbors.push({ node: [edgeNum, 0] });
      if (x.node[1] != 1) neighbors.push({ node: [edgeNum, 1] });

      if(edgeNum == to[0]){
        neighbors.push({ node: [edgeNum, to[1]] });
      }

      if (x.node[1] == 0) {
        if (edge.begin) {
          edge.begin.forEach(x => {            
            neighbors.push({ node: [x[0], x[1]] });
          });
        }
      }
      if (x.node[1] == 1) {
        if (edge.end) {
          edge.end.forEach(x => {            
            neighbors.push({ node: [x[0], x[1]] });
          });
        }
      }

      //console.log("getneighbors", x.node, neighbors);
      return neighbors;
    };
    var heuristic = (x) => {
      var edge = this.data.edges[x.node[0]];
      var n1 = this.data.nodes[edge.nodes[0]];
      var n2 = this.data.nodes[edge.nodes[1]];
      var v = [n2[0] - n1[0], n2[1] - n1[1]];
      var vl = Math.sqrt(v[0] * v[0] + v[1] * v[1]);
      v[0] = v[0] * x.node[1] / vl;
      v[1] = v[1] * x.node[1] / vl;

      var tedge = this.data.edges[to[0]];
      var tn1 = this.data.nodes[tedge.nodes[0]];
      var tn2 = this.data.nodes[tedge.nodes[1]];
      var tv = [tn2[0] - tn1[0], tn2[1] - tn1[1]];
      var tvl = Math.sqrt(tv[0] * tv[0] + tv[1] * tv[1]);
      tv[0] = tv[0] * to[1] / tvl;
      tv[1] = tv[1] * to[1] / tvl;

      var c = [v[0] - tv[0], v[1] - tv[1]];
      return Math.sqrt(c[0] * c[0] + c[1] * c[1])
    }
    var state = {
      openList: [{
        node: from,
        f: 99999,
        g: 0
      }],
      closedList: []
    };
    //output(state);

    var max = 5;
    while (max > 0 && state.openList.length > 0) {
      max--; if (max <= 0) break;
      //output(state);

      // Grab the lowest f(x) to process next
      var lowInd = 0;
      for (var i = 1; i < state.openList.length; i++) {
        if (state.openList[i].f < state.openList[lowInd].f) { lowInd = i; }
      }
      state.currentNode = state.openList[lowInd];
      state.openList.splice(lowInd, 1);

      //console.log("visiting", state.currentNode.node);

      // End case -- result has been found, return the traced path
      if (state.currentNode.node[0] == to[0] &&
        state.currentNode.node[1] == to[1]) {
        var curr = state.currentNode;
        var ret = [];
        while (curr.parent) {
          ret.push(curr);
          curr = curr.parent;
        }
        ret.push({ node: from });
        var final = ret.map(x => x.node).reverse();
        //console.log("SOLUTION", final);
        return final;
      }

      // Normal case -- move currentNode from open to closed, process each of its neighbors
      //state.openList.removeGraphNode(currentNode);
      state.closedList.push(state.currentNode);
      state.neighbors = getNeighbors(state.currentNode);

      for (var i = 0; i < state.neighbors.length; i++) {
        var neighbor = state.neighbors[i];
        //console.log("neighbor", i, neighbor.node);
        var indexClosedList = state.closedList.findIndex(x =>
          x.node[0] == neighbor.node[0] &&
          x.node[1] == neighbor.node[1]);
        if (indexClosedList >= 0 /*|| neighbor.isWall()*/) {
          // not a valid node to process, skip to next neighbor          
          //console.log("already on closed", neighbor.node);
          continue;
        }
        //g score is the shortest distance from start to current node, we need to check if
        //the path we have arrived at this neighbor is the shortest one we have seen yet
        var gScore = state.currentNode.g + 1; // 1 is the distance from a node to it's neighbor
        var gScoreIsBest = false;

        var index = state.openList.findIndex(x => x.node[0] == neighbor[0]);
        if (index < 0) {
          //console.log("first time seeing", neighbor.node);
          // This the the first time we have arrived at this node, it must be the best
          // Also, we need to take the h (heuristic) score since we haven't done so yet 
          gScoreIsBest = true;
          neighbor.h = heuristic(neighbor);
          //console.log("heuristic", neighbor.h);
          state.openList.push(neighbor);
        }
        else if (gScore < neighbor.g) {
          //console.log(gScore < neighbor.g);
          // We have already seen the node, but last time it had a worse g (distance from start)
          gScoreIsBest = true;
        }

        if (gScoreIsBest) {
          //console.log("gScoreIsBest");
          // Found an optimal (so far) path to this node.   Store info on how we got here and
          //  just how good it really is...
          neighbor.parent = state.currentNode;
          neighbor.g = gScore;
          neighbor.f = neighbor.g + neighbor.h;
          //console.log(neighbor.node, neighbor.f, neighbor.g, neighbor.h);
        }
      }
      state.neighbors = null;
      state.currentNode = null;
    }
    //output(state);
  };
  this.pushPath = (newNode) => {
    if (this.data.pos[0] == newNode[0]) {
      if (newNode[1] > this.data.pos[2]) {
        this.data.path.push([newNode[0], 0, newNode[1]]);
      } else {
        this.data.path.push([newNode[0], 1, newNode[1]]);
      }
    } else {
      var path = this.astar([this.data.pos[0], this.data.pos[1]], newNode);
      console.log(path);
      for (var i = 0; i < path.length; ++i) {
        var current = path[i];
        this.data.path.push([current[0], 0, current[1]]);
      }
      // var lastPos;
      // for (var i = 0; i < path.length; ++i) {
      //   var current = path[i];
      //   if (this.data.pos[0] == current[0]) {
      //     if (current[1] > this.data.pos[2]) {
      //       this.data.path.push([current[0], 0, current[1]]);
      //       lastPos = 1;
      //     } else {
      //       this.data.path.push([current[0], 1, current[1]]);
      //       lastPos = 0;
      //     }
      //   } else {
      //     var currentPos = lastPos == 0 ? 1 : 0;          
      //     this.data.path.push([current[0], currentPos, current[1]]);
      //     lastPos = currentPos;
      //   }
      // }
      // console.log(this.data.path);  
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

//var nodes = new Nodes();
//nodes.astart([0, 0], [1, 1]);
//nodes.astart([0, 0], [1, 0.5]);
//nodes.astart([0, 0], [0, 0.5]);