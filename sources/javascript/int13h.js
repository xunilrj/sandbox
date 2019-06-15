"use strict";

function firstLetterUpper(string) {
  return string.charAt(0).toUpperCase() + string.slice(1);
}

function firstLetterLower(string) {
  return string.charAt(0).toLocaleLowerCase() + string.slice(1);
}

function getColorRGB(color) {
    if (typeof color === 'string' || color instanceof String) {
      const d = document.createElement("div");
      document.body.appendChild(d);
      d.style.color = color;
      const c = window.getComputedStyle(d).color;
      document.body.removeChild(d);
      return c;
    }
    else if (color.length == 3) {
      let r = (color[0] * 255).toString(16);
      let g = (color[1] * 255).toString(16);
      let b = (color[2] * 255).toString(16);
      return `#${r}${g}${b}FF`;
    }
    else if (color.length == 4) {
      let r = (color[0] * 255).toString(16);
      let g = (color[1] * 255).toString(16);
      let b = (color[2] * 255).toString(16);
      let a = (color[3] * 255).toString(16);
      return `#${r}${g}${b}${a}`;
    }
    return color;
  }
  
  function lowContext(x, elDebug) {
    let obj;
    let canvas = x;
    let canvasParent = canvas.parentNode;

    let data = {
      w: 30,
      h: 30,
      hoverControl: "none"
    }
    let callbacks = {
      overPixel: null
    };
    const setData = (name, value) => {
      data[name] = value;
      var event = new CustomEvent('datachanged', {
        bubbles: true,
        detail: {name, value}
      });
      canvasParent.dispatchEvent(event);
    }

    
    var resolutionEl = document.createElement("div");
    resolutionEl.innerHTML = `<div>
    <button data-set-w="30" data-set-h="30">30x30</button>
    <button data-set-w="50" data-set-h="50">50x50</button>
    <button data-set-hover-control="'start'">Start</button>
</div>`;
    canvasParent.prepend(resolutionEl);
    canvasParent.addEventListener("click", e => {
      Object.keys(e.target.dataset).forEach(x => {
        if(x.startsWith("set")){
          var varName = firstLetterLower(x.substr(3, x.length - 3));
          var f = eval(`(x) => (${e.target.dataset[x]})`);
          setData(varName, f(data));
        }
      });
    });
    
    let ctx = canvas.getContext("2d");
  
    let mouseDown = false
    let mousePos = [0, 0];
    canvas.addEventListener("mousedown", e => {
      mouseDown = true;
    });
    canvas.addEventListener("mouseup", e => {
      mouseDown = false;
    });
    canvas.addEventListener("mousemove", e => {
      var r = canvas.getBoundingClientRect();
      mousePos[0] = e.clientX - r.x;
      mousePos[1] = e.clientY - r.y;
    });
  
    let start = 10;
    let pixelRadius = 5;
  
    let pixels = [];
    let objs = [];
    const render = (ts) => {
        pixels = []
        ctx.clearRect(0, 0, canvas.width, canvas.height);
  
        objs.forEach(x => {
            if (x.type == "line") {
                let sx = x.from[0] * 2 * pixelRadius + start;
                let sy = x.from[1] * 2 * pixelRadius + start;
                let ex = x.to[0] * 2 * pixelRadius + start;
                let ey = x.to[1] * 2 * pixelRadius + start;
        
                ctx.beginPath();
                ctx.moveTo(sx, sy);
                ctx.lineTo(ex, ey);
                ctx.stroke();
            }
            obj.drawLine(x.from[0], x.from[1], x.to[0], x.to[1], obj.putPixel);
        });
  
  
        ctx.setLineDash([5, 3]);
        ctx.strokeRect(0, 0,
           data.w * 2 * pixelRadius + (start),
           data.h * 2 * pixelRadius + (start));
        ctx.setLineDash([]);
        for (let x = 0; x <data.w; ++x) {
            for (let y = 0; y <data.h; ++y) {
            let cx = x * 2 * pixelRadius + start;
            let cy = y * 2 * pixelRadius + start;
            let sx = cx - pixelRadius;
            let ex = cx + pixelRadius;
            let sy = cy - pixelRadius;
            let ey = cy + pixelRadius;
            if (mousePos[0] >= sx && mousePos[0] < ex) {
                if (mousePos[1] >= sy && mousePos[1] < ey) {
                ctx.beginPath();
                ctx.arc(x * 2 * pixelRadius + start, y * 2 * pixelRadius + start,
                    pixelRadius, 0, 2 * Math.PI);
                ctx.fillStyle = "black";
                ctx.fill();
                }
            }
    
            ctx.beginPath();
            ctx.arc(cx, cy, pixelRadius - 2, 0, 2 * Math.PI);
            if (pixels[y *data.w + x]) {
                let c = pixels[y *data.w + x];
                ctx.fillStyle = c;
                ctx.fill();
            }
            else ctx.stroke();
            }
        }

        if(elDebug)
          elDebug.innerText = "";
        let gx = (mousePos[0] / 10).toFixed(0) - 1;
        let gy = (mousePos[1] / 10).toFixed(0) - 1;
        if (gx >= 0 && gx <data.w && gy >= 0 && gy <data.h)  {
            if (elDebug) elDebug.innerText = `${gx},${gy}`;
            if (callbacks.overPixel) callbacks.overPixel(gx, gy, [mouseDown]);
            setData("hover", [gx,gy]);
        }
        requestAnimationFrame(render);
    };
    requestAnimationFrame(render);
    let drawLine = (x0, x1, y0, y1, f) => {
  
    };
    let putPixel = (x, y, c) => {
      c = getColorRGB(c);
      pixels[y *data.w + x] = c;
    };
    obj = {
      drawLine,
      putPixel,
      addLine: (from, to) => {
        var l = {
          type: "line",
          from: from,
          to: to
        };
        objs.push(l);
        obj.drawLine(from[0], from[1], to[0], to[1], putPixel);
        return l;
      },
      addRadio: (name, options) => {
        //console.log(name, options);
        options.forEach(x => {
          console.log(x);
          var o = document.createElement("input");
          o.name = name;
          o.type = "radio";
          o.value = x;
          var l = document.createElement("label");
          l.innerText = x;
          l.append(o);
          canvasParent.firstElementChild.append(l);
        });
        return {
          get: () => {
            var c = canvasParent.querySelector(`input[name="${name}"]:checked`)
            if(c) return c.value;
            return null;
          },
          set: (value) => {
            var selector = canvasParent.querySelectorAll(`input[name="${name}"]`);
            Array.from(selector).forEach(x => x.checked = false);
            selector = `input[name="${name}"][value="${value}"]`;
            var el = canvasParent.querySelector(selector);
            if(el) el.checked = true;
          }
        };
      },
      overPixel: (f) => {
        callbacks.overPixel = f;
      }
    };
    return obj;
  }
  
  export default function getContext(x, type) {
    type = type || "int13h.low";
  
    let el;
    if (typeof x === 'string' || x instanceof String) {
      el = document.getElementById(x);
    } else if (x.tagName) {
      el = x;
    }
  
    if (!el) throw "element not found";
  
    if (el.tagName != "CANVAS") {
      el = el.querySelector("canvas");
    }
  
    let elDebug = document.getElementById(el.id + "Debug");
  
    if (type == "int13h.low") return lowContext(el, elDebug);
  }