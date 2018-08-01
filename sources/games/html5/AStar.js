
const getXY = (config, edge, t) => {
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

var heuristic = (config, to, x) => {
var v = getXY(config, to[0],to[1]);
var tv = getXY(config, x.node[0], x.node[1]);     

    var c = [v[0] - tv[0], v[1] - tv[1]];
    var result = Math.sqrt(c[0] * c[0] + c[1] * c[1])
    //console.log("heuristic", to, x.node, result);
    return result;
  }
  
var getNeighbors = (config, to, x) => {
    var edgeNum = x.node[0];
    var edge = config.edges[edgeNum];
    var neighbors = [];
    if ((x.node[1] > 0) && (x.node[1] < 1))
    {
      neighbors.push({ node: [edgeNum, 0] });
      neighbors.push({ node: [edgeNum, 1] });
    }
    if(edgeNum == to[0]) neighbors.push({ node: [to[0], to[1]] });      
    if (x.node[1] == 0) {
      var n = config.getNeighbors(edge.nodes[0]);        
      n.forEach(y => {
        if(y.edge == to[0]) neighbors.push({ node: [to[0], to[1]] });
        neighbors.push({ node: [y.edge, y.T + y.dir]});
      });
    } else if(x.node[1] == 1) {
      var n = config.getNeighbors(edge.nodes[1]);        
      n.forEach(y => {
        if(y.edge == to[0]) neighbors.push({ node: [to[0], to[1]] });
        neighbors.push({ node: [y.edge, y.T + y.dir]});
      });
    }
    //console.log("getNeighbots", to, x, neighbors);
    return neighbors;
  };



export default function (config, from, to) {
  //console.log("AStar",from, to);
  var state = {
    openList: [{
      node: from,
      f: 99999,
      g: 0
    }],
    closedList: []
  };

  var max = 100;
  while (max > 0 && state.openList.length > 0) {
    max--; if (max <= 0) break;

    // Grab the lowest f(x) to process next
    var lowInd = 0;
    for (var i = 1; i < state.openList.length; i++) {
      if (state.openList[i].f < state.openList[lowInd].f) { lowInd = i; }
    }
    state.currentNode = state.openList[lowInd];
    state.openList.splice(lowInd, 1);

    //console.log("currentNode", state.currentNode);

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
      //console.log(final);
      return final;
    }

    // Normal case -- move currentNode from open to closed, process each of its neighbors
    //state.openList.removeGraphNode(currentNode);
    state.closedList.push(state.currentNode);
    state.neighbors = getNeighbors(config, to, state.currentNode);

    for (var i = 0; i < state.neighbors.length; i++) {
      var neighbor = state.neighbors[i];
      var indexClosedList = state.closedList.findIndex(x =>
        x.node[0] == neighbor.node[0] &&
        x.node[1] == neighbor.node[1]);
      if (indexClosedList >= 0 /*|| neighbor.isWall()*/) {
        // not a valid node to process, skip to next neighbor          
        continue;
      }
      neighbor.g = heuristic(config, to, neighbor);
      //g score is the shortest distance 
      //from start to current node, we need to check if
      //the path we have arrived at this 
      //neighbor is the shortest one we have seen yet
      // 1 is the distance from a node to it's neighbor
      var gScore = state.currentNode.g 
        + heuristic(config, state.currentNode.node, neighbor); 
      var gScoreIsBest = false;

      var index = state.openList.findIndex(x => x.node[0] == neighbor[0]);
      if (index < 0) {
        // This the the first time we have arrived 
        //at this node, it must be the best
        // Also, we need to take 
        //the h (heuristic) score since we haven't done so yet 
        gScoreIsBest = true;
        neighbor.h = heuristic(config, to, neighbor);
        state.openList.push(neighbor);
      }
      else if (gScore < neighbor.g) {
        // We have already seen the node,
        // but last time it had a worse g (distance from start)
        gScoreIsBest = true;
      }

      if (gScoreIsBest) {
        // Found an optimal (so far) path to this node.
        // Store info on how we got here and
        // just how good it really is...
        neighbor.parent = state.currentNode;
        neighbor.g = gScore;
        neighbor.f = neighbor.g + neighbor.h;          
      }
    }
    state.neighbors = null;
    state.currentNode = null;
  }
}