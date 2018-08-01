function loadImage(url) {
  return new Promise(resolve => {
    let i = new Image();
    i.onload = () => { resolve(i) };
    i.src = url;
  });
}

function loadFile(url) {
  return fetch(url);
}

export default function (obj) {
  var preData = obj.init({
    loadImage: loadImage
  });
  var el = document.getElementById("console");
  Promise.all(preData.assets)
    .then(result => {
      var kernel = {
        loadImage: loadImage,
        loadFile: loadFile,
        log: (x) => {
          if (el.children.length >= 10) {
            el.removeChild(el.firstChild);
          }
          var ellog = document.createElement('pre');
          if (arguments.length == 1)
            ellog.innerHTML = JSON.stringify(x);
          else
            ellog.innerHTML = JSON.stringify(Array.from(arguments));
          el.appendChild(ellog);
        },
        mouseClicks: [],
        mousePos: [],
        cursor: Object.assign({
          default: false,
          asset: null,
          adjust: [0,0]
        }, preData.cursor)
      };
      var assets = Object.assign(
        Object.create(kernel), { assets: result });
      var components = Object.assign(
        Object.create(assets), { components: preData.components });
      var data = Object.create(components);

      const canvas = document.getElementById("canvas");
      const ctx = canvas.getContext("2d");
      obj.graphics.context = ctx;
      ctx = obj.graphics;

      let lastTimestamp = null;
      function render(timestamp) {
        if (!lastTimestamp) lastTimestamp = timestamp - 16;
        const dt = (timestamp - lastTimestamp) / 1000.0;
        lastTimestamp = timestamp;

        obj.update(dt, data);
        obj.render(ctx, data);

        data.components.forEach(x => {
          x.update(dt, data);
        });
        data.components.forEach(x => {
          x.render(ctx, data);
        });

        obj.graphics.render(data);

        if (data.cursor && 
          !data.cursor.default && 
          data.mousePos[0] && 
          data.cursor.asset) {       
          var asset = data.assets[data.cursor.asset];
          if(asset){
            obj.graphics.context.drawImage(            
              asset,
              data.mousePos[0].pos[0] + data.cursor.adjust[0],
              data.mousePos[0].pos[1] + data.cursor.adjust[1]);
          }
        }

        kernel.mouseClicks = [];
        document.animId = requestAnimationFrame(render);
      }
      if (document.eventListeners) {
        document.eventListeners.forEach(x => {
          document.removeEventListener(x[0], x[1]);
        });
      }
      document.eventListeners = [];
      const addListener = (event, f) => {
        document.addEventListener(event, f);
        document.eventListeners.push([event, f]);
      }
      const getCursorPosition = (event) => {
        var rect = canvas.getBoundingClientRect();
        var x = event.clientX - rect.left;
        var y = event.clientY - rect.top;
        return [x, y];
      }
      addListener("click", (e) => {
        kernel.mouseClicks.push({
          type: "left",
          pos: getCursorPosition(e)
        });
      });
      addListener("mousemove", (e) => {
        kernel.mousePos = [{ pos: getCursorPosition(e) }];
      });
      if (document.animId) {
        cancelAnimationFrame(document.animId);
      }
      document.animId = requestAnimationFrame(render);
    });
}