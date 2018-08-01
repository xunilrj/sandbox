import AStar from 'AStar.js';
import GetNearestPoint from 'GetNearestPoint.js';

export default function WalkGraph(config, setf, events) {
  this.events = [];
  if (events) this.events.push(events);

  this.callback = setf || [];
  this.config = config;
  const mode = 0;
  const getXY = (edge, t) => {
    if (!this.config) return [0, 0];
    if (t < 0) t = 0;
    if (t > 1) t = 1;
    var e = this.config.edges[edge];
    var n0 = this.config.nodes[e.nodes[0]];
    var n1 = this.config.nodes[e.nodes[1]];
    var v = [n1.pos[0] - n0.pos[0], n1.pos[1] - n0.pos[1]];
    return [
      v[0] * t + n0.pos[0],
      v[1] * t + n0.pos[1]];
  };

  const posEdge = 0;
  const posT = 0;
  const posDir = 0;
  const posXY = getXY(0, 0);
  this.setConfig = (config) => {
    this.config = config;
    posXY = getXY(0, 0);
  }
  this.setPos = (pos) => {
    posXY = getXY(pos[0], pos[1]);
    posEdge = pos[0];
    posT = pos[1];

    if (pos[1] == 1 && this.config.edges[posEdge].end)
      posInfo = Object.assign(posInfo, this.config.edges[posEdge].end);
    else if (pos[1] == 0 && this.config.edges[posEdge].begin)
      posInfo = Object.assign(posInfo, this.config.edges[posEdge].begin);
  }
  const posInfo = {};
  const getIncidence = (x, y) => this.config.incidence[y * this.config.nodes.length + x];
  const getCommonNode = (e1, e2) => {
    if (this.config.edges[e1].nodes[0] == this.config.edges[e2].nodes[0])
      return this.config.edges[e2].nodes[0];
    else if (this.config.edges[e1].nodes[0] == this.config.edges[e2].nodes[1])
      return this.config.edges[e2].nodes[1];
    else if (this.config.edges[e1].nodes[1] == this.config.edges[e2].nodes[0])
      return this.config.edges[e2].nodes[0];
    else if (this.config.edges[e1].nodes[1] == this.config.edges[e2].nodes[1])
      return this.config.edges[e2].nodes[1];
  }
  const targets = [];
  const pushTargets = (path) => {
    //var shiftFirst = false;
    //if(posEdge == path[0][0] && posT == path[0][1])
    //  shiftFirst = true;

    if (targets.length == 0) {
      posDir = 1;
      posEdge = path[0][0];
      posT = path[0][1];

      if (path.length > 1) {
        var next = path[1][0];
        if (posEdge == next) {
          posDir = path[1][1] > posT ? 1 : -1;
        } else {
          var common = getCommonNode(posEdge, next);
          var currentEdge = this.config.edges[posEdge];
          if (common == currentEdge.nodes[0]) {
            posDir = -1;
            path[0][1] = 0;
          } else {
            posDir = 1;
            path[0][1] = 1;
          }
        }
      }
      //console.log(posDir, posEdge, posT);
    }
    //console.log(path);
    targets.push(...path);
  };
  this.walkTo = (point) => {
    targets = [];
    var result = AStar(this.config, [posEdge, posT], point);
    pushTargets(result);
    //console.log(posEdge,posT,point,result);
  };
  const nextTarget = () => {
    var currentTarget = targets[0];
    var currentEdge = currentTarget[0];
    if (currentTarget[1] == 1 && this.config.edges[currentEdge].end)
      posInfo = Object.assign(posInfo, this.config.edges[currentEdge].end);
    else if (currentTarget[1] == 0 && this.config.edges[currentEdge].begin)
      posInfo = Object.assign(posInfo, this.config.edges[currentEdge].begin);
    targets.shift();
    if (targets.length > 0) {
      var nextPosEdge = targets[0][0];
      if (nextPosEdge != posEdge) {
        var at = this.config.edges[posEdge].nodes[1];
        if (posDir == -1)
          at = this.config.edges[posEdge].nodes[0];
        var target = this.config.edges[nextPosEdge].nodes[1];
        if (this.config.edges[nextPosEdge].nodes[1] == at) {
          target = this.config.edges[nextPosEdge].nodes[0];
        }
        var incidence = getIncidence(at, target);
        posT = incidence.T;
        posDir = incidence.dir;
        posEdge = nextPosEdge;
      } else {
        posDir = targets[0][1] > posT ? 1 : -1;
      }
    }

    if(targets.length == 0){
      this.events.forEach(f => {
        f({type:"end"});
      });  
    }
  };
  this.update = (dt, data) => {
    if (!this.config) return;



    if (targets.length > 0) {
      posT += dt * posDir;
      if (posDir == 1) {
        var limit = (targets[0] && targets[0][1]) || 1;
        if (posT >= limit) {
          posT = limit;
          nextTarget();
        }
      }
      else if (posDir == -1) {
        var limit = (targets[0] && targets[0][1]) || 0;
        if (posT <= limit) {
          posT = limit;
          nextTarget();
        }
      }
    }
    var oldPosXY = posXY;
    posXY = getXY(posEdge, posT);

    var moving = false;
    var dirCode = "";
    var dir = [posXY[0] - oldPosXY[0], posXY[1] - oldPosXY[1]];
    if (dir[1] > +0.0001) dirCode = "Down";
    else if (dir[1] < -0.0001) dirCode = "Up";
    if (dir[0] > +0.0001) dirCode += "Right";
    else if (dir[0] < -0.0001) dirCode += "Left";
    if (Math.abs(dir[0]) > 0.0001) moving = true;
    else if (Math.abs(dir[1]) > 0.0001) moving = true;
    if (this.callback) {
      //data.log({pos:posXY,dir:dirCode,moving:moving,info:posInfo})
      this.callback.forEach(f => f({
        pos: posXY,
        dir: dirCode,
        moving: moving,
        info: posInfo
      }));
    }

    /*if (data.mouseClicks.length > 0) {
      var point = data.mouseClicks[0].pos
      var best = GetNearestPoint(point, this.config);
      this.walkTo([best[0], best[4]]);
    }*/
  };
  this.render = (ctx, data) => {
    if (!this.config) return;
    ctx.strokeStyle = "White";
    ctx.fillStyle = "Red";
    let i = 0;
    this.config.nodes.forEach(x => {
      ctx.beginPath();
      ctx.arc(x.pos[0], x.pos[1], 2, 0, 2 * Math.PI);
      ctx.fill();
      ctx.font = "bold 18px Arial";
      ctx.fillText(i.toString(), x.pos[0], x.pos[1]);
      ++i;
    });

    i = 0;
    this.config.edges.forEach(x => {
      const n0 = this.config.nodes[x.nodes[0]];
      const n1 = this.config.nodes[x.nodes[1]];
      ctx.beginPath();
      ctx.moveTo(n0.pos[0], n0.pos[1]);
      ctx.lineTo(n1.pos[0], n1.pos[1]);
      ctx.stroke();

      var middle = [
        (n1.pos[0] - n0.pos[0]) * 0.8 + n0.pos[0],
        (n1.pos[1] - n0.pos[1]) * 0.8 + n0.pos[1],
      ];

      ctx.font = "bold 10px Arial";
      ctx.fillText(i.toString(), middle[0], middle[1]);
      ++i;
    });
    ctx.fillStyle = "red";
    ctx.beginPath();
    ctx.arc(posXY[0], posXY[1], 4, 0, 2 * Math.PI);
    ctx.fill();
  }
}