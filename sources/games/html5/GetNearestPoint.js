import { myclamp, vec, vecscal, vecdot, vecminus, vecLength, vecNormalize, vecDraw } from 'Math.js';

export default function GetNearestPoint(point, config, ctx) {
  var best = null;
  for (var i = 0; i < config.edges.length; ++i) {
    var current = config.edges[i];
    var nodes = current.nodes;

    var node1 = config.nodes[nodes[0]].pos;
    var node2 = config.nodes[nodes[1]].pos;

    var vece = vec(node1, node2);
    var veceL = vecLength(vece);
    var veceN = vec(node1, node2, true);
    var vec1 = vec(node1, point);
    var vec2 = vec(node2, point);

    var along = vecdot(vec1, veceN);
    var normalizedAlong = myclamp(along / veceL, 0.0, 1.0);
    var projVec1 = vecscal(vece, normalizedAlong);
    var vecDist = vecminus(projVec1, vec1);

    //vecDraw(ctx, node1, projVec1, "Red");
    //vecDraw(ctx, point, vecDist, "Red");

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