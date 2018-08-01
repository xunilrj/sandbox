export default function BoxManager(config, callback, events) {
  this.config = config;
  this.setConfig = (config) => {
    this.config = config;
    var i = 0;
    this.data.order = config.layers.map(x => [i++, x.z]);
  }
  const getPoints = (layer, box) => {
    if (!layer.object) return box.points;
    var op = layer.object.drawData.pos;
    var ad = layer.object.adjust || [0, 0];
    var p = box.points;
    return [op[0] + p[0] + ad[0], op[1] + p[1] + ad[1], p[2], p[3]];
  };
  this.pushLayer = (layer) => {
    var z = layer.z;
    var i = this.config.layers.length;
    this.config.layers.push(layer);
    this.data.order.push([i, layer.z]);
  }

  this.callback = [];
  if (callback) this.callback.push(callback);

  this.events = [];
  if (events) this.callback.push(events);

  this.data = {
    cursorPos: [0, 0],
    clickPos: [0, 0],
    inside: {

    }
  };

  const isInside = (x, bp) => {
    return x[0] >= bp[0] &&
      x[0] <= (bp[0] + bp[2]) &&
      x[1] >= bp[1] &&
      x[1] <= (bp[1] + bp[3]);
  }

  const notInside = (ilayer, ibox, box, entering, exiting) => {
    if (!this.data.inside[ilayer + "." + ibox]) return;
    delete this.data.inside[ilayer + "." + ibox];
    exiting.push([ilayer, ibox]);
  };
  const inside = (ilayer, ibox, box, entering, exiting) => {
    if (this.data.inside[ilayer + "." + ibox]) return;
    this.data.inside[ilayer + "." + ibox] = box;
    entering.push([ilayer, ibox]);
  };
  this.update = (dt, data) => {
    if (!this.config) return;

    if (data.mousePos.length) {
      this.data.cursorPos = data.mousePos[0].pos;
    }
    this.data.clickPos = null;
    if (data.mouseClicks.length) {
      this.data.clickPos = data.mouseClicks[0].pos;
    }

    var entering = [];
    var exiting = [];

    this.data.order.sort((l, r) => { return r[1] - l[1] });
    this.data.order.forEach(o => {
      var ilayer = o[0];
      var layer = this.config.layers[ilayer];
      if (layer.enabled) {
        if (layer.object && !layer.object.enable) return;
        var boxes = layer.boxes;
        var ibox = 0;
        return boxes.forEach(b => {
          var bp = getPoints(layer, b);
          if (isInside(this.data.cursorPos, bp)) {
            inside(ilayer, ibox, b, entering, exiting);
          } else {
            notInside(ilayer, ibox, b, entering, exiting);
          }
          ibox++;
        });
      }
    });



    ////////////////////////CLICK
    this.data.clickInside = null;
    if (this.data.clickPos) {
      this.data.order.some(o => {
        var ilayer = o[0];
        var layer = this.config.layers[ilayer];
        if (layer.enabled) {
          if (layer.object && !layer.object.enable) return;
          var boxes = layer.boxes;
          return boxes.some(b => {
            var bp = getPoints(layer, b);
            if (isInside(this.data.clickPos, bp)) {
              if (layer.object)
                this.data.clickObject = layer.object;
              this.data.clickLayer = layer;
              this.data.clickInside = b;
              return true;
            }
          });
        }
      });
    }

    ////////UPDATE

    this.callback.forEach(f => f(this.data));

    ///////////////// mouse over

    entering.forEach(x => {
      this.events.forEach(f => f({ type: "enterBox" }));
      var layer = this.config.layers[x[0]];
      if (layer && layer.object) {
        this.events.forEach(f => f({ type: "enterObjectBox" }));
        this.events.forEach(f => {
          f({ type: "enterObjectBox." + layer.object.def.name });
        });
      }
    });

    exiting.forEach(x => {
      this.events.forEach(f => f({ type: "exitBox" }));
      var layer = this.config.layers[x[0]];
      if (layer && layer.object) {
        this.events.forEach(f => f({ type: "exitObjectBox" }));
        this.events.forEach(f => {
          f({ type: "exitObjectBox." + layer.object.def.name });
        });
      }
    });

    //////////////////// click

    if (this.data.clickInside) {
      var stopped = false;

      if (this.data.clickInside.clickRaise) {
        stopped = this.events.some(f => {
          return this.data.clickInside.clickRaise.some(e => {
            return f(e) || false;
          });
        });
      }

      if (!stopped && this.data.clickLayer && this.data.clickLayer.object) {
        stopped = this.events.some(f => {
          var name = this.data.clickLayer.object.def.name;
          return f({ type: "clickObjectkBox." + name }) || false;
        });

        if (!stopped) {
          stopped = this.events.some(f => {
            return f({ type: "clickObjectkBox" }) || false;
          });
        }
      }

      if (!stopped) {
        this.events.some(f => {
          return f({ type: "clickBox" }) || false;
        });
      }
    }
    //////////////
  };
  this.render = (ctx, data) => {
    if (!this.config) return;
    this.data.order.forEach(o => {
      var ilayer = o[0];
      var layer = this.config.layers[ilayer];
      if (layer.enabled) {
        if (layer.object && !layer.object.enable) return;
        var boxes = layer.boxes;
        boxes.forEach(b => {
          var bp = getPoints(layer, b);
          ctx.rect(bp[0], bp[1], bp[2], bp[3]);
          ctx.stroke();
        });
      }
    });
  }
}