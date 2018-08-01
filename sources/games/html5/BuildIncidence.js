export default function (nodes, edges) {
  const l = nodes.length;
  const m = Array(l * l).fill({});
  const set = (x, y, i) => m[y * l + x] = i;
  let i = 0;
  edges.forEach(e => {
    var n0 = e.nodes[0];
    var n1 = e.nodes[1];
    set(n0, n1, { edge: i, dir: 1, T: 0 });
    set(n1, n0, { edge: i, dir: -1, T: 1 });
    ++i;
  });
  return {
    nodes: nodes,
    edges: edges,
    incidence: m,
    getIncidence: (x, y) => config.incidence[y * config.nodes.length + x],
    getNeighbors: (x) => {
      var p = [];
      for (var y = 0; y < nodes.length; ++y) {
        var i = m[y * nodes.length + x];
        if (i.dir) p.push(i);
      }
      return p;
    }
  };
}