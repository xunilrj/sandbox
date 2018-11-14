

var canvas = document.getElementById("canvas");
var ctx = canvas.getContext("2d");

var imgs = {
}
var limbs = [];
var ianim = 27;
var anim;
var draw = false;
var iframe = 0;
var tframe = 0;
fetch("http://127.0.0.1:8080/aksq_json.txt")
  .then(x => x.json())
  .then(x => {
    var defs = x[ianim].Defs;
    console.log(defs);
    defs = Object.keys(defs)
      .map(x => defs[x])
      .filter(x => x.Mode == 6 || x.Mode == 5);
    var ps = defs.map(d => {
      console.log(d);
      var url = "http://127.0.0.1:8080/anim_" + d.Start + ".json";
      return fetch(url);
    });
    return Promise.all(ps);
  })
  .then(defs => Promise.all(defs.map(x => x.json())))
  .then(defs => {
    var i = 0;
    var ps = defs.map(x => {
      x.Operations = x.Operations
        .filter(o => o && o.Name && o.Items && o.Items[0].Frame);
      var frames = x.Operations
        .map(o => o.Items[0].Frame)
        .filter((x, i, a) => a.indexOf(x) == i);

      limbs.push({
        def: x,
        tframe: 0,
        iframe: 0
      });

      var ps = frames.map(f => {
        var res, rej;
        var p = new Promise((a, b) => { res = a; rej = b; });
        var img = new Image();
        img.onload = function () {
          imgs[f] = img;
          res(img);
        };
        img.src = 'http://127.0.0.1:8080/costume_2_frame_' + f + '_.png';
        return p;
      });
      return Promise.all(ps);
    });
    return Promise.all(ps);
  })
  .then(x => {
    console.log(limbs);
    draw = true;
  });

var pos = [100, 300];

var lastTimestamp;

function render(timestamp) {
  ctx.clearRect(0, 0, canvas.width, canvas.height);

  if (!lastTimestamp) lastTimestamp = timestamp - 16;
  var dt = (timestamp - lastTimestamp) / 1000;
  lastTimestamp = timestamp;

  limbs.forEach(x => {
    x.tframe += dt;
    if (draw) {      
      if (x.tframe >= 0.1) {
        x.iframe = (x.iframe+1) % x.def.Operations.length;
        x.tframe = 0;
      }
      op = x.def.Operations[x.iframe];
      if (op) {
        if (op.Name == "Jump") {
          x.iframe = 0;
        }
        if (op.Name == "SetAnimVar") {
          x.iframe = (x.iframe++) % x.def.Operations.length;
          console.log(op);
        }
        var op = x.def.Operations[x.iframe];
        if (op.Items) {
          var i = op.Items[0].Frame;
          var img = imgs[i];
          ctx.drawImage(img,
            pos[0] + op.Items[0].OffsetX,
            pos[1] + op.Items[0].OffsetY);
        }
      }
    }

  });

  requestAnimationFrame(render);
};
requestAnimationFrame(render);