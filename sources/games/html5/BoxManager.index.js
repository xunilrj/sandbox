import start from 'Game.js';
import Graphics from 'Graphics.js';
import BoxManager from 'BoxManager.js';
import boxConfig from "BoxConfig.js";

var boxManager = new BoxManager(boxConfig, x => {
  if(x.type == "exit") console.log("enter");
});
start({
  graphics: new Graphics(),
  init: x => {
    return {
      assets: [
        x.loadImage("http://localhost:8081/mi3/Room009/00.png")
      ],
      components: [
        boxManager
      ],
    };
  },
  update: (dt, data) => {
  },
  render: (ctx, data) => {
    ctx.drawImage(data.assets[0], 0, 0);
  }
});
