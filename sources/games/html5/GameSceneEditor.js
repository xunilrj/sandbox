import start from 'Game.js';
import GameScene from 'GameScene.js';
import Graphics from 'Graphics.js';
import store from 'App.js';

function readSingleFile(el) {
  var file = el.files[0];
  if (!file) {
    return;
  }
  var p = new Promise((a, b) => {
    var reader = new FileReader();
    reader.onload = function (e) {
      var contents = e.target.result;
      a(contents);
    };
    reader.readAsText(file);
  });

  return p;
}

const app = store();
app.handle("click");
app.addFunction("RestartScene", x => {
  fetch("http://localhost:8081/mi3/gamedata/Game.json",
  {cache: "no-store"})
    .then(x => x.json())
    .then(x => {
      var room = x.rooms.cannon;
      scene.restart({
        scripts: room.scripts,
        objects: x.objects
      });
    });
});

const scene = new GameScene();
start({
  graphics: new Graphics(),
  init: x => {
    return {
      assets: [
      ],
      components: [
        scene
      ],
    };
  },
  update: (dt, data) => {
  },
  render: (ctx, data) => {
  }
});
