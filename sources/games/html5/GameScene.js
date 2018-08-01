import Sprite from 'Sprite.js';
import buildIncidence from 'BuildIncidence.js';
import WalkGraph from 'WalkGraph.js';
import BoxManager from 'BoxManager.js';
import GetNearestPoint from 'GetNearestPoint.js';

export default function GameScene() {
  const data = {};
  const lastBoxManager;
  const lastObject;
  const lastGraph;
  const lastClick;
  const userStack;
  const zero = (x) => {
    lastBoxManager = null;
    lastObject = null;
    lastGraph = null;
    lastClick = null;
    userStack = [];
    data = {
      firstTime: true,
      scripts: {
        entry: ""
      },
      objects: {

      },
      objectTable: x.objects || []
    };
    if (x && x.scripts) {
      Object.keys(x.scripts)
        .forEach(k => data.scripts[k] = x.scripts[k]);
    }
  };
  zero({});
  const obj2asset = {

  };
  const getPos = id => {
    var obj = data.objects[id];
    console.log(obj);
    if (obj.def.type == "Picture") {
      return obj.drawData.pos;
    }
  }
  const setPos = (id, x, y) => {
    const obj = data.objects[id];
    if (obj.gateway.d && obj.gateway.d.data) {
      obj.gateway.d.data.pos = [x, y];
    }
    else {
      obj.drawData.pos = [x, y];
    }
  }
  const getValue = (name, gamedata) => {
    if (name == "click.x") {
      return lastClick[0];
    } else if (name == "click.y") {
      return lastClick[1];
    } else if (name == "lastObject") {
      return lastObject.name;
    } else if (name == "lastObject.x") {
      return getPos(lastObject.name)[0];
    } else if (name == "lastObject.y") {
      return getPos(lastObject.name)[1];
    } else if (name == "userPop") {
      var top = userStack.pop();
      console.log("userStack", userStack);
      return top;
    }
    return name;
  },
  const runScript = (name, gamedata) => {
    var stopped = false;
    const script = data.scripts[name];
    if (!script) {
      console.log("runningScript", name, "notFound");
      return stopped;
    }
    console.log("runningScript", name, script.code);
    var lines = script.code;
    const operandTable = {
      loadObj: x => {
        let gateway = { d: null };
        const id = x[0];
        const enable = x[1] == "true";
        if (!x[1]) enable = true;
        const loadBoxes = x[2] == "true";
        if (!x[2]) loadBoxes = true;

        const def = data.objectTable[id];
        data.objects[id] = {
          enable: false,
          def: def,
          gateway: gateway,
          drawData: {
            frame: null,
            scale: null,
            pos: [0, 0],
            z: 0,
          },
          update: (dt, gamedata) => {
            //gamedata.log(gateway.d)
            if (gateway.d && gateway.d.update) {
              gateway.d.update(dt, gamedata);
            }
          },
          render: (ctx, gamedata) => {
            //gamedata.log(def, gateway.d)
            if (gateway.d && gateway.d.render) {
              gateway.d.render(ctx, gamedata);
            }
            else if (def.type == "Picture" && def.picture) {
              const me = data.objects[id].drawData;

              if (me && me.frame) {
                //gamedata.log(me);
                ctx.batchDraw(me.z, g => {
                  g.save();
                  g.translate(me.pos[0], me.pos[1]);
                  var rect = me.frame.rect;
                  if (me.scale) {
                    g.translate(rect[2], 0);
                    g.scale(-1, 1);
                  }
                  g.drawImage(def.picture,
                    rect[0], rect[1], rect[2], rect[3],
                    0, 0, rect[2], rect[3]);
                  g.restore();
                });
              }
              else {
                var adjust = data.objects[id].adjust || [0, 0]
                ctx.batchDraw(me.z, g => {
                  var pos = me.pos || [0, 0];
                  g.drawImage(def.picture,
                    pos[0] + adjust[0], pos[1] + adjust[0]);
                });
              }
            }
          }
        }

        if (def.type == "Picture") {
          var length = gamedata.assets.length;
          gamedata.assets.push(null);
          obj2asset[id] = length;
          gamedata.loadImage(def.file).then((loadResult) => {
            data.objects[id].enable = enable;
            def.picture = loadResult;
            gamedata.assets[obj2asset[id]] = loadResult;
          });
          data.objects[id].adjust = def.adjust;
        }
        else if (def.type == "Sprite") {
          gateway.d = new Sprite(null, null, {});
          gamedata.loadFile(def.file)
            .then(result => result.json())
            .then(result => {
              def.config = result;
              gateway.d.config = result;
              data.objects[id].enable = enable;
            });
        }
        else if (def.type == "Graph") {
          gateway.d = new WalkGraph(null);
          lastGraph = gateway.d;
          gamedata.loadFile(def.file)
            .then(result => result.json())
            .then(result => {
              def.config = result;
              gateway.d.setConfig(buildIncidence(
                result.nodes,
                result.edges));
              gateway.d.setPos([0, 0]);
              gateway.d.events.push(e => {
                runScript("walkToEnd", gamedata);

                var lo = getValue("lastObject");
                if (lo) {
                  var name = "walkToObjectEnd";
                  runScript(name, gamedata);

                  var name = "walkToObjectEnd." + lo;
                  runScript(name, gamedata);
                }
              });
              data.objects[id].enable = enable;
            });
        } else if (def.type == "BoxManager") {
          gateway.d = new BoxManager();
          lastBoxManager = gateway.d;
          if (def.config) {
            gateway.d.setConfig(def.config);
            gateway.d.events.push(e => {
              return runScript(e.type, gamedata);
            });
            gateway.d.callback.push(x => {
              if (x.clickPos)
                lastClick = x.clickPos;
              if (x.clickObject) {
                lastObject = x.clickObject.def;
              }
            });
            data.objects[id].enable = enable;
          }
          else {
            console.log("missing config", def);
          }
        }

        if (def.boxLayer) {
          def.boxLayer.object = data.objects[id];
        }

        if (def.boxLayer && loadBoxes) {

          lastBoxManager.pushLayer(def.boxLayer);
        }
      },
      bind: x => {
        const aid = x[0];
        const bid = x[1];
        const a = data.objects[aid];
        const b = data.objects[bid];
        if (!a) {
          console.log("bind error - cannotfind", aid);
          return;
        }
        if (!b) {
          console.log("bind error - cannotfind", bid);
          return;
        }
        if (a.def.type == "Picture" && b.def.type == "Sprite") {
          b.gateway.d.callback.push(x => {
            a.drawData = x;
          });
        }
        else if (a.def.type == "Sprite" && b.def.type == "Graph") {
          b.gateway.d.callback.push(x => {
            //gamedata.log(x);
            const sprite = a.gateway.d;
            var pos = x.pos;
            sprite.data.pos = [pos[0] - 40, pos[1] - 250];

            var animationName = "standing";
            if (!x.dir) x.dir = "DownLeft";
            if (x.moving) animationName = "walking";
            sprite.data.animation = animationName + x.dir;

            if (x.info.z) sprite.data.z = x.info.z
          });
        }
      },
      startAnim: x => {
        const id = x[0];
        const anim = x[1];
        const obj = data.objects[id];
        obj.gateway.d.startAnim(anim);
      },
      setZ: x => {
        const id = x[0];
        const z = x[1];
        const obj = data.objects[id];
        if (obj.gateway.d && obj.gateway.d.data) {
          obj.gateway.d.data.z = z;
        } else {
          obj.drawData.z = z;
        }
      },
      setPos: x => {
        const id = x[0];
        const xx = parseInt(getValue(x[1], gamedata));
        const yy = parseInt(getValue(x[2], gamedata));
        setPos(id, xx, yy);
      },
      setSamePos: x => {
        const target = getValue(x[0]);
        const source = getValue(x[1]);
        const pos = getPos(source);
        setPos(target, pos[0], pos[1]);
      },
      showCursor: x => {
        const flag = x[0];
        const obj = x[1];
        gamedata.cursor = {
          default: false,
          asset: obj2asset[obj],
          adjust: [-35, -25]
        }
      },
      enableObj: x => {
        const id = x[0];
        const flag = x[1];
        const obj = data.objects[id];
        obj.enable = flag.toLowerCase() == "true";
      },
      log: x => {
        var values = x.map(y => getValue(y));
        console.log("scriptLog", values);
      },
      invoke: x => {
        var name = x.map(y => getValue(y)).join("");
        runScript(name, gamedata);
      },
      walkTo: x => {
        const id = getValue(x[0]);
        var point = getPos(id);
        var best = GetNearestPoint(point, lastGraph.config);
        lastGraph.walkTo([best[0], best[4]]);
      },
      userPush: x => {
        const value = getValue(x[0]);
        userStack.push(value);
        console.log("userStack", userStack);
      },
      userPop: x => {
        userStack.pop();
        console.log("userStack", userStack);
      },
      stopPropagation: x => {
        stopped = true;
      }
    };
    lines.forEach(l => {
      if (l.startsWith("//")) {
        return;
      }
      const operands = l.split(" ")
      const f = operandTable[operands[0]];
      operands.shift();
      f(operands);
    });
    return stopped;
  };
  this.restart = (x) => {
    zero(x);
  };
  this.update = (dt, gamedata) => {
    if (data.firstTime == true) {
      runScript("entry", gamedata);
      data.firstTime = false;
    }

    Object.keys(data.objects)
      .forEach(x => {
        const current = data.objects[x];
        if (current && current.enable && current.update) {
          current.update(dt, gamedata);
        }
      });
  };
  this.render = (ctx, gamedata) => {
    Object.keys(data.objects)
      .forEach(x => {
        const current = data.objects[x];
        if (current && current.enable && current.render) {
          current.render(ctx, gamedata);
        }
      });
  }
};