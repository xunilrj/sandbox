export default function store () {
  const domProxy = (el) => {
    return new Proxy({}, {
      get: (target, prop) => {
        if (el && el.dataset && el.dataset[prop])
          return el.dataset[prop]
        var el2 = document.getElementById(el.id + "." + prop);
        if (el2) {
          return el2.value;
        } else {
          var el3 = document.getElementById(prop);
          if (el3) {
            if (el3.tagName === "OL") {
              return Array.from(el3.children)
                .map(c => JSON.parse(c.dataset.obj));
            }
            else if (el3.tagName === "INPUT") {
              if (el3.type.toLowerCase() === "file") {
                return readSingleFile(el3);
              }
            }
            else return el3.value;
          }
        }
      }
    });
  };
  const listeners = {
  };
  const functions = {

  };
  const views = {

  };
  const target = {
    handle: (event) => {
      document.addEventListener(event, x => {
        if (functions[x.target.id]) {
          const p = domProxy(x.target);
          functions[x.target.id](p, x);
        }
      });
    },
    when: (path, to) => {
      if (listeners[path]) {
        listeners[path].push(to);
      } else {
        listeners[path] = [to];
      }
    },
    set: (path, value) => {
      if (listeners[path]) {
        listeners[path].forEach(x => {
          if (x === document) {
            const el = document.getElementById(path);
            if (el) {
              if (el.tagName === "TEXTAREA") {
                el.value = value;
              } else if (el.tagName === "INPUT") {
                el.value = value;
              }
            }
          }
        });
      }
    },
    replace: (path, index, value) => {
      if (listeners[path]) {
        listeners[path].forEach(x => {
          if (x === document) {
            const el = document.getElementById(path);
            if (el) {
              if (el.tagName === "OL") {
                var li = el.children[index];
                if (views[path]) {
                  var viewEl = views[path](value);
                  li.replaceChild(viewEl, li.firstChild);
                }
                else {
                  li.innerHTML = value;
                }
                li.dataset.obj = JSON.stringify(value);
              }
            }
          }
        });
      }
    },
    push: (path, value) => {
      if (listeners[path]) {
        listeners[path].forEach(x => {
          if (x === document) {
            const el = document.getElementById(path);
            if (el) {
              if (el.tagName === "TEXTAREA") {
                el.innerHTML = value;
              } else if (el.tagName === "OL") {
                const li = document.createElement("li");

                if (views[path]) {
                  var viewEl = views[path](value);
                  li.appendChild(viewEl);
                }
                else {
                  li.innerHTML = value;
                }
                li.dataset.obj = JSON.stringify(value);

                el.appendChild(li);
              }
            }
          }
        });
      }
    },
    addFunction: (name, f) => {
      functions[name] = f;
    },
    addView: (name, f) => {
      views[name] = f;
    }
  };
  return target;
}

const elementify = (content) => {
  if (content instanceof Array) {
    return content.map(x => {
      return elementify(x)[0];
    });
  }
  else if (typeof content === 'string' || content instanceof String) {
    var span = document.createElement("span");
    span.innerText = content;
    return [span];
  }
  else if (typeof content === 'object' || content instanceof HTMLElement) {
    return [content];
  }
}
const div = (content) => {
  const el = document.createElement("div");
  elementify(content).forEach(x => {
    el.appendChild(x);
  });
  return el;
}
const button = (content, attrs) => {
  const el = document.createElement("button");
  el.id = attrs.id;
  el.innerText = content.toString();
  Object.assign(el.dataset, attrs.dataset);
  return el;
}