// modules are defined as an array
// [ module function, map of requires ]
//
// map of requires is short require name -> numeric require
//
// anything defined in a previous bundle is accessed via the
// orig method which is the require for previous bundles
parcelRequire = (function (modules, cache, entry, globalName) {
  // Save the require from previous bundle to this closure if any
  var previousRequire = typeof parcelRequire === 'function' && parcelRequire;
  var nodeRequire = typeof require === 'function' && require;

  function newRequire(name, jumped) {
    if (!cache[name]) {
      if (!modules[name]) {
        // if we cannot find the module within our internal map or
        // cache jump to the current global require ie. the last bundle
        // that was added to the page.
        var currentRequire = typeof parcelRequire === 'function' && parcelRequire;
        if (!jumped && currentRequire) {
          return currentRequire(name, true);
        }

        // If there are other bundles on this page the require from the
        // previous one is saved to 'previousRequire'. Repeat this as
        // many times as there are bundles until the module is found or
        // we exhaust the require chain.
        if (previousRequire) {
          return previousRequire(name, true);
        }

        // Try the node require function if it exists.
        if (nodeRequire && typeof name === 'string') {
          return nodeRequire(name);
        }

        var err = new Error('Cannot find module \'' + name + '\'');
        err.code = 'MODULE_NOT_FOUND';
        throw err;
      }

      localRequire.resolve = resolve;
      localRequire.cache = {};

      var module = cache[name] = new newRequire.Module(name);

      modules[name][0].call(module.exports, localRequire, module, module.exports, this);
    }

    return cache[name].exports;

    function localRequire(x){
      return newRequire(localRequire.resolve(x));
    }

    function resolve(x){
      return modules[name][1][x] || x;
    }
  }

  function Module(moduleName) {
    this.id = moduleName;
    this.bundle = newRequire;
    this.exports = {};
  }

  newRequire.isParcelRequire = true;
  newRequire.Module = Module;
  newRequire.modules = modules;
  newRequire.cache = cache;
  newRequire.parent = previousRequire;
  newRequire.register = function (id, exports) {
    modules[id] = [function (require, module) {
      module.exports = exports;
    }, {}];
  };

  var error;
  for (var i = 0; i < entry.length; i++) {
    try {
      newRequire(entry[i]);
    } catch (e) {
      // Save first error but execute all entries
      if (!error) {
        error = e;
      }
    }
  }

  if (entry.length) {
    // Expose entry point to Node, AMD or browser globals
    // Based on https://github.com/ForbesLindesay/umd/blob/master/template.js
    var mainExports = newRequire(entry[entry.length - 1]);

    // CommonJS
    if (typeof exports === "object" && typeof module !== "undefined") {
      module.exports = mainExports;

    // RequireJS
    } else if (typeof define === "function" && define.amd) {
     define(function () {
       return mainExports;
     });

    // <script>
    } else if (globalName) {
      this[globalName] = mainExports;
    }
  }

  // Override the current require with this new one
  parcelRequire = newRequire;

  if (error) {
    // throw error from earlier, _after updating parcelRequire_
    throw error;
  }

  return newRequire;
})({"../../../../../../../root/.nvm/versions/node/v12.4.0/lib/node_modules/parcel/src/builtins/bundle-url.js":[function(require,module,exports) {
var bundleURL = null;

function getBundleURLCached() {
  if (!bundleURL) {
    bundleURL = getBundleURL();
  }

  return bundleURL;
}

function getBundleURL() {
  // Attempt to find the URL of the current script and use that as the base URL
  try {
    throw new Error();
  } catch (err) {
    var matches = ('' + err.stack).match(/(https?|file|ftp|chrome-extension|moz-extension):\/\/[^)\n]+/g);

    if (matches) {
      return getBaseURL(matches[0]);
    }
  }

  return '/';
}

function getBaseURL(url) {
  return ('' + url).replace(/^((?:https?|file|ftp|chrome-extension|moz-extension):\/\/.+)\/[^/]+$/, '$1') + '/';
}

exports.getBundleURL = getBundleURLCached;
exports.getBaseURL = getBaseURL;
},{}],"../../../../../../../root/.nvm/versions/node/v12.4.0/lib/node_modules/parcel/src/builtins/css-loader.js":[function(require,module,exports) {
var bundle = require('./bundle-url');

function updateLink(link) {
  var newLink = link.cloneNode();

  newLink.onload = function () {
    link.remove();
  };

  newLink.href = link.href.split('?')[0] + '?' + Date.now();
  link.parentNode.insertBefore(newLink, link.nextSibling);
}

var cssTimeout = null;

function reloadCSS() {
  if (cssTimeout) {
    return;
  }

  cssTimeout = setTimeout(function () {
    var links = document.querySelectorAll('link[rel="stylesheet"]');

    for (var i = 0; i < links.length; i++) {
      if (bundle.getBaseURL(links[i].href) === bundle.getBundleURL()) {
        updateLink(links[i]);
      }
    }

    cssTimeout = null;
  }, 50);
}

module.exports = reloadCSS;
},{"./bundle-url":"../../../../../../../root/.nvm/versions/node/v12.4.0/lib/node_modules/parcel/src/builtins/bundle-url.js"}],"style.css":[function(require,module,exports) {
var reloadCSS = require('_css_loader');

module.hot.dispose(reloadCSS);
module.hot.accept(reloadCSS);
},{"_css_loader":"../../../../../../../root/.nvm/versions/node/v12.4.0/lib/node_modules/parcel/src/builtins/css-loader.js"}],"node_modules/nu-stream/dist_node/stream.js":[function(require,module,exports) {
/*
 * THIS FILE IS AUTO GENERATED FROM 'lib/stream.kep'
 * DO NOT EDIT
*/
"use strict";
var end, NIL, stream, memoStream, rec, cons, append, appendz, concat, bind, from, first, rest, isEmpty, isStream,
        reverse, foldl, foldr, reduce, reduceRight, zip, zipWith, indexed, map, filter, forEach, toArray, arrayReduce =
        Function.prototype.call.bind(Array.prototype.reduce),
    memo = (function(f) {
        var value;
        return (function() {
            if ((value === undefined)) {
                (value = f());
            }
            return value;
        });
    });
(end = null);
(NIL = null);
(stream = (function(val, f) {
    return ({
        first: val,
        rest: f
    });
}));
(memoStream = (function(val, f) {
    var f0 = memo(f);
    return ({
        first: val,
        rest: f0
    });
}));
(rec = (function(def) {
    var value = def((function() {
        return value;
    }));
    return value;
}));
(first = (function(x) {
    return x.first;
}));
(rest = (function(s) {
    return s.rest();
}));
(isEmpty = (function(y) {
    return (null === y);
}));
(isStream = (function(s) {
    return (((s && s.hasOwnProperty("first")) && s.hasOwnProperty("rest")) || (null === s));
}));
(cons = (function(val, s) {
    var f = (function() {
        return s;
    });
    return ({
        first: val,
        rest: f
    });
}));
(appendz = (function(s1, f) {
    var val, f0, f1;
    return ((null === s1) ? f() : ((val = s1.first), (f0 = (function() {
        return appendz(s1.rest(), f);
    })), (f1 = memo(f0)), ({
        first: val,
        rest: f1
    })));
}));
var reducer = (function(s1, s2) {
    return appendz(s1, (function() {
        return s2;
    }));
});
(append = (function() {
    var streams = arguments;
    return arrayReduce(streams, reducer, null);
}));
(concat = (function(s) {
    return ((null === s) ? s : appendz(s.first, (function() {
        return concat(s.rest());
    })));
}));
var fromImpl = (function(arr, i, len) {
    var val, f, f0;
    return ((i >= len) ? null : ((val = arr[i]), (f = (function() {
        return fromImpl(arr, (i + 1), len);
    })), (f0 = memo(f)), ({
        first: val,
        rest: f0
    })));
});
(from = (function(arr) {
    var length = arr["length"],
        val, f, f0;
    return ((0 >= length) ? null : ((val = arr[0]), (f = (function() {
        return fromImpl(arr, 1, length);
    })), (f0 = memo(f)), ({
        first: val,
        rest: f0
    })));
}));
(zipWith = (function(f, l1, l2) {
    var val, f0, f1;
    return (((null === l1) || (null === l2)) ? null : ((val = f(l1.first, l2.first)), (f0 = zipWith.bind(null,
        f, l1.rest(), l2.rest())), (f1 = memo(f0)), ({
        first: val,
        rest: f1
    })));
}));
var f = (function(x, y) {
    return [x, y];
});
(zip = (function(l1, l2) {
    var x, y, val, f0, f1;
    return (((null === l1) || (null === l2)) ? null : ((x = l1.first), (y = l2.first), (val = [x, y]), (f0 =
        zipWith.bind(null, f, l1.rest(), l2.rest())), (f1 = memo(f0)), ({
        first: val,
        rest: f1
    })));
}));
var count = (function(n) {
    var f0 = (function() {
        return count((n + 1));
    });
    return ({
        first: n,
        rest: f0
    });
}),
    f0;
(indexed = zip.bind(null, ((f0 = (function() {
    return count(1);
})), ({
    first: 0,
    rest: f0
}))));
(foldl = (function(f1, z, s) {
    var y, s0, r = z;
    for (var head = s;
        (!((y = head), (null === y)));
        (head = ((s0 = head), s0.rest()))) {
        var x;
        (r = f1(r, ((x = head), x.first)));
    }
    return r;
}));
(reverse = foldl.bind(null, (function(x, y) {
    var f1 = (function() {
        return x;
    });
    return ({
        first: y,
        rest: f1
    });
}), null));
(foldr = (function(f1, z, s) {
    return foldl(f1, z, reverse(s));
}));
(reduce = (function(f1, s) {
    return foldl(f1, s.first, s.rest());
}));
(reduceRight = (function(f1, s) {
    return reduce(f1, reverse(s));
}));
(map = (function(f1, s) {
    var val, f2, f3;
    return ((null === s) ? s : ((val = f1(s.first)), (f2 = (function() {
        return map(f1, s.rest());
    })), (f3 = memo(f2)), ({
        first: val,
        rest: f3
    })));
}));
(filter = (function(pred, s) {
    var y, s0;
    for (var head = s;
        (!((y = head), (null === y)));
        (head = ((s0 = head), s0.rest()))) {
        var x = head,
            x0 = x.first;
        if (pred(x0)) {
            var f1 = (function() {
                var s1;
                return filter(pred, ((s1 = head), s1.rest()));
            }),
                f2 = memo(f1);
            return ({
                first: x0,
                rest: f2
            });
        }
    }
    return null;
}));
var y = concat;
(bind = (function() {
    var args = arguments;
    return y(map.apply(null, args));
}));
(forEach = (function(f1, s) {
    var y0, s0, x;
    for (var head = s;
        (!((y0 = head), (null === y0)));
        (head = ((s0 = head), s0.rest()))) f1(((x = head), x.first));
}));
var builder = (function(p, c) {
    p.push(c);
    return p;
});
(toArray = (function(s) {
    return foldl(builder, [], s);
}));
(exports["end"] = end);
(exports["NIL"] = NIL);
(exports["stream"] = stream);
(exports["memoStream"] = memoStream);
(exports["rec"] = rec);
(exports["cons"] = cons);
(exports["append"] = append);
(exports["appendz"] = appendz);
(exports["concat"] = concat);
(exports["bind"] = bind);
(exports["from"] = from);
(exports["first"] = first);
(exports["rest"] = rest);
(exports["isEmpty"] = isEmpty);
(exports["isStream"] = isStream);
(exports["reverse"] = reverse);
(exports["foldl"] = foldl);
(exports["foldr"] = foldr);
(exports["reduce"] = reduce);
(exports["reduceRight"] = reduceRight);
(exports["zip"] = zip);
(exports["zipWith"] = zipWith);
(exports["indexed"] = indexed);
(exports["map"] = map);
(exports["filter"] = filter);
(exports["forEach"] = forEach);
(exports["toArray"] = toArray);
},{}],"node_modules/nu-stream/dist_node/gen.js":[function(require,module,exports) {
/*
 * THIS FILE IS AUTO GENERATED FROM 'lib/gen.kep'
 * DO NOT EDIT
*/
"use strict";
var __o = require("./stream"),
    repeat, range, NIL = __o["NIL"],
    memoStream = __o["memoStream"];
(repeat = (function(times, x) {
    return ((times <= 0) ? NIL : memoStream(x, (function() {
        return repeat((times - 1), x);
    })));
}));
var rangeImpl = (function(lower, upper, step) {
    return (((step > 0) ? (upper <= lower) : (upper >= lower)) ? NIL : memoStream(lower, (function() {
        return rangeImpl((lower + step), upper, step);
    })));
});
(range = (function(lower, upper, step) {
    var rangeLower = (isNaN(lower) ? Infinity : (+lower)),
        rangeStep = (isNaN(step) ? 1 : (+step));
    return (isNaN(upper) ? (((rangeStep > 0) ? (rangeLower <= 0) : (rangeLower >= 0)) ? NIL : memoStream(0, (
        function() {
            return rangeImpl((0 + rangeStep), rangeLower, rangeStep);
        }))) : (((rangeStep > 0) ? (upper <= rangeLower) : (upper >= rangeLower)) ? NIL : memoStream(
        rangeLower, (function() {
            return rangeImpl((rangeLower + rangeStep), upper, rangeStep);
        }))));
}));
(exports["repeat"] = repeat);
(exports["range"] = range);
},{"./stream":"node_modules/nu-stream/dist_node/stream.js"}],"node_modules/nu-stream/dist_node/quantifier.js":[function(require,module,exports) {
/*
 * THIS FILE IS AUTO GENERATED FROM 'lib/quantifier.kep'
 * DO NOT EDIT
*/
"use strict";
var __o = require("./stream"),
    any, every, isEmpty = __o["isEmpty"],
    first = __o["first"],
    rest = __o["rest"],
    not = (function(y) {
        return (function(z) {
            var x = y(z);
            return (!x);
        });
    });
(any = (function(pred, s) {
    for (var current = s;
        (!isEmpty(current));
        (current = rest(current)))
        if (pred(first(current))) return true;
    return false;
}));
(every = (function(pred, s) {
    return (!any(not(pred), s));
}));
(exports["any"] = any);
(exports["every"] = every);
},{"./stream":"node_modules/nu-stream/dist_node/stream.js"}],"node_modules/nu-stream/dist_node/select.js":[function(require,module,exports) {
/*
 * THIS FILE IS AUTO GENERATED FROM 'lib/select.kep'
 * DO NOT EDIT
*/
"use strict";
var __o = require("./stream"),
    takeWhile, take, skipWhile, skip, NIL = __o["NIL"],
    first = __o["first"],
    isEmpty = __o["isEmpty"],
    map = __o["map"],
    stream = __o["stream"],
    rest = __o["rest"],
    indexed = __o["indexed"],
    value = (function(__o0) {
        var x = __o0[1];
        return x;
    });
(takeWhile = (function(pred, s) {
    var x;
    return (isEmpty(s) ? s : ((x = first(s)), (pred(x) ? stream(x, (function() {
        return takeWhile(pred, rest(s));
    })) : NIL)));
}));
(take = (function(count, s) {
    return ((isNaN(count) || (count < 0)) ? s : map(value, takeWhile((function(z) {
        var i = z[0];
        return (count > i);
    }), indexed(s))));
}));
(skipWhile = (function(pred, s) {
    for (var head = s;
        (!isEmpty(head));
        (head = rest(head)))
        if ((!pred(first(head)))) return head;
    return NIL;
}));
(skip = (function(count, s) {
    return ((isNaN(count) || (count <= 0)) ? s : map(value, skipWhile((function(z) {
        var i = z[0];
        return (count > i);
    }), indexed(s))));
}));
(exports["takeWhile"] = takeWhile);
(exports["take"] = take);
(exports["skipWhile"] = skipWhile);
(exports["skip"] = skip);
},{"./stream":"node_modules/nu-stream/dist_node/stream.js"}],"node_modules/nu-stream/index.js":[function(require,module,exports) {
module.exports = {
    'stream': require('./dist_node/stream'),
    'gen': require('./dist_node/gen'),
    'quantifier': require('./dist_node/quantifier'),
    'select': require('./dist_node/select')
};
},{"./dist_node/stream":"node_modules/nu-stream/dist_node/stream.js","./dist_node/gen":"node_modules/nu-stream/dist_node/gen.js","./dist_node/quantifier":"node_modules/nu-stream/dist_node/quantifier.js","./dist_node/select":"node_modules/nu-stream/dist_node/select.js"}],"node_modules/seshet/dist_node/seshet.js":[function(require,module,exports) {
/*
 * THIS FILE IS AUTO GENERATED from 'lib/seshet.kep'
 * DO NOT EDIT
*/
"use strict";
var create, lookup, update, prune;
var max = (function(x, y) {
    return ((x > y) ? x : y);
});
var heightFromChild = (function(child) {
    return (child ? (1 + child.height) : 0);
});
var height = (function(root) {
    return (!root ? 0 : max(heightFromChild(root.left), heightFromChild(root.right)));
});
var bf = (function(node) {
    return (!node ? 0 : (heightFromChild(node.left) - heightFromChild(node.right)));
});
var Cell = (function(id, val, delegate) {
    (this.id = id);
    (this.val = val);
    (this.delegate = delegate);
});
(Cell.lookup = (function(base, eq, id) {
    for (var cell = base; cell;
        (cell = cell.delegate))
        if (eq(cell.id, id)) return cell.val;
    return null;
}));
var Node = (function(key, cell, l, r, height) {
    (this.key = key);
    (this.cell = cell);
    (this.left = l);
    (this.right = r);
    (this.height = height);
});
(Node.setChildren = (function(node, l, r) {
    return new(Node)(node.key, node.cell, l, r, ((l || r) ? (1 + max(height(l), height(r))) : 0));
}));
(Node.setLeft = (function(node, l) {
    return Node.setChildren(node, l, node.right);
}));
(Node.setRight = (function(node, r) {
    return Node.setChildren(node, node.left, r);
}));
(Node.lookup = (function(root, compare, eq, key, id) {
    for (var node = root; node;) {
        var diff = compare(key, node.key);
        if ((diff === 0)) return Cell.lookup(node.cell, eq, id);
        (node = ((diff < 0) ? node.left : node.right));
    }
    return null;
}));
(Node.put = (function(node, id, val) {
    return new(Node)(node.key, new(Cell)(id, val, node.cell), node.left, node.right, node.height);
}));
var rr = (function(node) {
    return (!node ? node : Node.setLeft(node.right, Node.setRight(node, node.right.left)));
});
var ll = (function(node) {
    return (!node ? node : Node.setRight(node.left, Node.setLeft(node, node.left.right)));
});
var lr = (function(node) {
    return ll(Node.setLeft(node, rr(node.left)));
});
var rl = (function(node) {
    return rr(Node.setRight(node, ll(node.right)));
});
var rot = (function(node) {
    var d = bf(node);
    if ((d > 1)) return ((bf(node.left) <= -1) ? lr(node) : ll(node));
    else if ((d < -1)) return ((bf(node.right) >= 1) ? rl(node) : rr(node));
    return node;
});
(Node.update = (function(root, compare, key, id, val) {
    if (!root) return new(Node)(key, new(Cell)(id, val, null), null, null, 0);
    var diff = compare(key, root.key);
    if ((diff === 0)) return Node.put(root, id, val);
    return rot(((diff < 0) ? Node.setLeft(root, Node.update(root.left, compare, key, id, val)) : Node.setRight(
        root, Node.update(root.right, compare, key, id, val))));
}));
(Node.rebalance = (function(root) {
    return ((Math.abs(bf(root)) <= 1) ? root : rot(Node.setChildren(root, Node.rebalance(root.left), Node.rebalance(
        root.right))));
}));
(Node.prune = (function(root, compare, lower, upper) {
    if (!root) return root;
    if ((lower !== undefined)) {
        var dl = compare(root.key, lower);
        if ((dl < 0)) return Node.prune(root.right, compare, lower, upper);
        else if ((dl === 0)) return Node.setChildren(root, null, Node.prune(root.right, compare, undefined,
            upper));
    }
    if (((upper !== undefined) && (compare(root.key, upper) >= 0))) return Node.prune(root.left, compare, lower,
        upper);
    return Node.setChildren(root, Node.prune(root.left, compare, lower, upper), Node.prune(root.right, compare,
        lower, upper));
}));
var Memoer = (function(compare, eq, root) {
    (this.compare = compare);
    (this.eq = eq);
    (this.root = root);
});
(Memoer.setRoot = (function(m, root) {
    return new(Memoer)(m.compare, m.eq, root);
}));
(create = (function() {
        var equals = (function(x, y) {
            return (x === y);
        });
        return (function(compare, eq) {
            return new(Memoer)(compare, (eq || equals), null);
        });
    })
    .call(this));
(lookup = (function(m, key, id) {
    return Node.lookup(m.root, m.compare, m.eq, key, id);
}));
(update = (function(m, key, id, val) {
    return Memoer.setRoot(m, Node.update(m.root, m.compare, key, id, val));
}));
(prune = (function(m, lower, upper) {
    return Memoer.setRoot(m, Node.rebalance(Node.prune(m.root, m.compare, lower, upper)));
}));
(exports.create = create);
(exports.lookup = lookup);
(exports.update = update);
(exports.prune = prune);
},{}],"node_modules/bennu/dist_node/parse.js":[function(require,module,exports) {
/*
 * THIS FILE IS AUTO GENERATED FROM 'lib/parse.kep'
 * DO NOT EDIT
*/
"use strict";
var stream = require("nu-stream")["stream"],
    seshat = require("seshet"),
    Tail, trampoline, ParserError, ParseError, MultipleError, UnknownError, UnexpectError, ExpectError, ParserState,
        Position, Parser, label, late, rec, unparser, always, of, never, bind, chain, map, ap, extract, getParserState,
        setParserState, modifyParserState, getState, setState, modifyState, getInput, setInput, getPosition,
        setPosition, fail, attempt, look, lookahead, next, sequences, sequencea, sequence, empty, either, concat,
        choices, choicea, choice, optional, expected, not, eager, binds, cons, append, enumerations, enumerationa,
        enumeration, many, many1, manyTill, memo, token, anyToken, eof, exec, parseState, parseStream, parse, runState,
        runStream, run, testState, testStream, test, NIL = stream["NIL"],
    first = stream["first"],
    isEmpty = stream["isEmpty"],
    rest = stream["rest"],
    reduceRight = stream["reduceRight"],
    foldr = stream["foldr"],
    identity = (function(x) {
        return x;
    }),
    args = (function() {
        var args0 = arguments;
        return args0;
    });
(Tail = (function(p, state, m, cok, cerr, eok, eerr) {
    var self = this;
    (self.p = p);
    (self.state = state);
    (self.m = m);
    (self.cok = cok);
    (self.cerr = cerr);
    (self.eok = eok);
    (self.eerr = eerr);
}));
(trampoline = (function(f) {
    var value = f;
    while ((value instanceof Tail)) {
        (value = value.p(value.state, value.m, value.cok, value.cerr, value.eok, value.eerr));
    }
    return value;
}));
var Memoer = (function(memoer, frames) {
    var self = this;
    (self.memoer = memoer);
    (self.frames = frames);
});
(Memoer.empty = new(Memoer)(seshat.create((function(x, y) {
    return x.compare(y);
}), (function(x, y) {
    return ((x.id === y.id) && ((x.state === y.state) || (x.state && x.state.eq(y.state))));
})), NIL));
(Memoer.pushWindow = (function(m, lower) {
    return new(Memoer)(m.memoer, stream.cons(lower, m.frames));
}));
(Memoer.popWindow = (function(m) {
    var frames = m["frames"],
        r = rest(frames);
    return new(Memoer)((isEmpty(r) ? seshat.prune(m.memoer, first(frames)) : m.memoer), r);
}));
(Memoer.prune = (function(m, position) {
    return (isEmpty(m.frames) ? new(Memoer)(seshat.prune(m.memoer, position), m.frames) : m);
}));
(Memoer.lookup = (function(m, pos, id) {
    return seshat.lookup(m.memoer, pos, id);
}));
(Memoer.update = (function(m, pos, id, val) {
    return new(Memoer)(seshat.update(m.memoer, pos, id, val), m.frames);
}));
(Position = (function(i) {
    var self = this;
    (self.index = i);
}));
(Position.initial = new(Position)(0));
(Position.prototype.toString = (function() {
    var self = this;
    return ("" + self.index);
}));
(Position.prototype.increment = (function(_, _0) {
    var self = this;
    return new(Position)((self.index + 1));
}));
(Position.prototype.compare = (function(pos) {
    var self = this;
    return (self.index - pos.index);
}));
(ParserState = (function(input, position, userState) {
    var self = this;
    (self.input = input);
    (self.position = position);
    (self.userState = userState);
}));
(ParserState.prototype.eq = (function(other) {
    var self = this;
    return ((other && (self.input === other.input)) && (self.userState === other.userState));
}));
(ParserState.prototype.isEmpty = (function() {
    var self = this;
    return isEmpty(self.input);
}));
(ParserState.prototype.first = (function() {
    var self = this;
    return first(self.input);
}));
(ParserState.prototype.next = (function(tok) {
    var self = this;
    if ((!self._next)) {
        var r = rest(self.input),
            s = new(ParserState)(r, self.position.increment(tok, r), self.userState);
        (self._next = new(Parser)((function(_, m, cok) {
            return cok(tok, s, m);
        })));
    }
    return self._next;
}));
(ParserState.prototype.setInput = (function(input) {
    var self = this;
    return new(ParserState)(input, self.position, self.userState);
}));
(ParserState.prototype.setPosition = (function(position) {
    var self = this;
    return new(ParserState)(self.input, position, self.userState);
}));
(ParserState.prototype.setUserState = (function(userState) {
    var self = this;
    return new(ParserState)(self.input, self.position, userState);
}));
(ParserError = (function(msg) {
    var self = this;
    (self.message = msg);
}));
(ParserError.prototype = new(Error)());
(ParserError.prototype.constructor = ParserError);
(ParserError.prototype.name = "ParserError");
(ParseError = (function(position, msg) {
    var self = this;
    (self.position = position);
    (self._msg = (msg || ""));
}));
(ParseError.prototype = new(Error)());
(ParseError.prototype.constructor = ParseError);
(ParseError.prototype.name = "ParseError");
(ParseError.prototype.toString = (function() {
    var self = this;
    return self.message;
}));
Object.defineProperties(ParseError.prototype, ({
    message: ({
        configurable: true,
        get: (function() {
            var self = this;
            return ((("At " + self.position) + " ") + self.errorMessage);
        })
    }),
    errorMessage: ({
        configurable: true,
        get: (function() {
            var self = this;
            return self._msg;
        })
    })
}));
(MultipleError = (function(position, errors) {
    var self = this;
    (self.position = position);
    (self.errors = (errors || []));
}));
(MultipleError.prototype = new(ParseError)());
(MultipleError.prototype.constructor = MultipleError);
(MultipleError.prototype.name = "MultipleError");
Object.defineProperty(MultipleError.prototype, "errorMessage", ({
    get: (function() {
        var self = this;
        return (("[" + self.errors.map((function(x) {
                return x.message;
            }))
            .join(", ")) + "]");
    })
}));
var ChoiceError = (function(position, pErr, qErr) {
    var self = this;
    (self.position = position);
    (self._pErr = pErr);
    (self._qErr = qErr);
});
(ChoiceError.prototype = new(MultipleError)());
(ChoiceError.prototype.constructor = MultipleError);
(ChoiceError.prototype.name = "ChoiceError");
Object.defineProperty(ChoiceError.prototype, "errors", ({
    get: (function() {
        var self = this;
        return [self._pErr].concat(self._qErr.errors);
    })
}));
(UnknownError = (function(position) {
    var self = this;
    (self.position = position);
}));
(UnknownError.prototype = new(ParseError)());
(UnknownError.prototype.constructor = UnknownError);
(UnknownError.prototype.name = "UnknownError");
Object.defineProperty(UnknownError.prototype, "errorMessage", ({
    value: "unknown error"
}));
(UnexpectError = (function(position, unexpected) {
    var self = this;
    (self.position = position);
    (self.unexpected = unexpected);
}));
(UnexpectError.prototype = new(ParseError)());
(UnexpectError.prototype.constructor = UnexpectError);
(UnexpectError.prototype.name = "UnexpectError");
Object.defineProperty(UnexpectError.prototype, "errorMessage", ({
    get: (function() {
        var self = this;
        return ("unexpected: " + self.unexpected);
    })
}));
(ExpectError = (function(position, expected, found) {
    var self = this;
    (self.position = position);
    (self.expected = expected);
    (self.found = found);
}));
(ExpectError.prototype = new(ParseError)());
(ExpectError.prototype.constructor = ExpectError);
(ExpectError.prototype.name = "ExpectError");
Object.defineProperty(ExpectError.prototype, "errorMessage", ({
    get: (function() {
        var self = this;
        return (("expected: " + self.expected) + (self.found ? (" found: " + self.found) : ""));
    })
}));
(Parser = (function(n) {
    var self = this;
    (self.run = n);
}));
(unparser = (function(p, state, m, cok, cerr, eok, eerr) {
    return new(Tail)(p.run, state, m, cok, cerr, eok, eerr);
}));
(label = (function(name, p) {
    return (p.run.hasOwnProperty("displayName") ? label(name, new(Parser)((function(state, m, cok, cerr, eok,
        eerr) {
        return new(Tail)(p.run, state, m, cok, cerr, eok, eerr);
    }))) : new(Parser)(Object.defineProperty(p.run, "displayName", ({
        value: name,
        writable: false
    }))));
}));
(late = (function(def) {
    var value;
    return new(Parser)((function(state, m, cok, cerr, eok, eerr) {
        (value = (value || def()));
        var p = value;
        return new(Tail)(p.run, state, m, cok, cerr, eok, eerr);
    }));
}));
(rec = (function(def) {
    var value = def(late((function() {
        return value;
    })));
    return value;
}));
(Parser.prototype.of = (function(x) {
    return new(Parser)((function(state, m, _, _0, eok, _1) {
        return eok(x, state, m);
    }));
}));
(Parser.of = Parser.prototype.of);
(of = Parser.of);
(always = of);
(never = (function(x) {
    return new(Parser)((function(state, m, _, _0, _1, eerr) {
        return eerr(x, state, m);
    }));
}));
(Parser.chain = (function(p, f) {
    return new(Parser)((function(state, m, cok, cerr, eok, eerr) {
        var cok0 = (function(x, state0, m0) {
            var p0 = f(x);
            return new(Tail)(p0.run, state0, m0, cok, cerr, cok, cerr);
        }),
            eok0 = (function(x, state0, m0) {
                var p0 = f(x);
                return new(Tail)(p0.run, state0, m0, cok, cerr, eok, eerr);
            });
        return new(Tail)(p.run, state, m, cok0, cerr, eok0, eerr);
    }));
}));
(chain = Parser.chain);
(bind = chain);
(Parser.prototype.chain = (function(f) {
    var self = this;
    return chain(self, f);
}));
(Parser.prototype.map = (function(f) {
    var self = this;
    return chain(self, (function(z) {
        return of(f(z));
    }));
}));
(Parser.map = (function(f, p) {
    return p.map(f);
}));
(map = Parser.map);
(Parser.ap = (function(f, m) {
    return chain(f, (function(f0) {
        return m.map(f0);
    }));
}));
(ap = Parser.ap);
(Parser.prototype.ap = (function(m2) {
    var self = this;
    return ap(self, m2);
}));
(modifyParserState = (function(f) {
    return new(Parser)((function(state, m, _, _0, eok, _1) {
        var newState = f(state);
        return eok(newState, newState, m);
    }));
}));
var p = new(Parser)((function(state, m, _, _0, eok, _1) {
    return eok(state, state, m);
}));
(getParserState = (p.run.hasOwnProperty("displayName") ? label("Get Parser State", new(Parser)((function(state, m, cok,
    cerr, eok, eerr) {
    return new(Tail)(p.run, state, m, cok, cerr, eok, eerr);
}))) : new(Parser)(Object.defineProperty(p.run, "displayName", ({
    value: "Get Parser State",
    writable: false
})))));
(setParserState = (function(z) {
    return new(Parser)((function(state, m, _, _0, eok, _1) {
        return eok(z, z, m);
    }));
}));
(extract = (function(f) {
    return new(Parser)((function(state, m, _, _0, eok, _1) {
        return eok(f(state), state, m);
    }));
}));
(modifyState = (function(f) {
    return new(Parser)((function(state, m, _, _0, eok, _1) {
        var newState = state.setUserState(f(state.userState));
        return eok(newState, newState, m);
    }));
}));
var p0 = new(Parser)((function(state, m, _, _0, eok, _1) {
    return eok(state.userState, state, m);
}));
(getState = (p0.run.hasOwnProperty("displayName") ? label("Get State", new(Parser)((function(state, m, cok, cerr, eok,
    eerr) {
    return new(Tail)(p0.run, state, m, cok, cerr, eok, eerr);
}))) : new(Parser)(Object.defineProperty(p0.run, "displayName", ({
    value: "Get State",
    writable: false
})))));
(setState = (function(z) {
    return new(Parser)((function(state, m, _, _0, eok, _1) {
        var newState = state.setUserState(z);
        return eok(newState, newState, m);
    }));
}));
var p1 = new(Parser)((function(state, m, _, _0, eok, _1) {
    return eok(state.position, state, m);
}));
(getPosition = (p1.run.hasOwnProperty("displayName") ? label("Get Position", new(Parser)((function(state, m, cok, cerr,
    eok, eerr) {
    return new(Tail)(p1.run, state, m, cok, cerr, eok, eerr);
}))) : new(Parser)(Object.defineProperty(p1.run, "displayName", ({
    value: "Get Position",
    writable: false
})))));
(setPosition = (function(position) {
    return new(Parser)((function(state, m, _, _0, eok, _1) {
        var newState = state.setPosition(position);
        return eok(newState, newState, m);
    }));
}));
var p2 = new(Parser)((function(state, m, _, _0, eok, _1) {
    return eok(state.input, state, m);
}));
(getInput = (p2.run.hasOwnProperty("displayName") ? label("Get Input", new(Parser)((function(state, m, cok, cerr, eok,
    eerr) {
    return new(Tail)(p2.run, state, m, cok, cerr, eok, eerr);
}))) : new(Parser)(Object.defineProperty(p2.run, "displayName", ({
    value: "Get Input",
    writable: false
})))));
(setInput = (function(input) {
    return new(Parser)((function(state, m, _, _0, eok, _1) {
        var newState = state.setInput(input);
        return eok(newState, newState, m);
    }));
}));
(fail = (function(msg) {
    var e = (msg ? ParseError : UnknownError);
    return chain(getPosition, (function(z) {
        var x = new(e)(z, msg);
        return new(Parser)((function(state, m, _, _0, _1, eerr) {
            return eerr(x, state, m);
        }));
    }));
}));
(attempt = (function(p3) {
    return new(Parser)((function(state, m, cok, cerr, eok, eerr) {
        var peerr = (function(x, s, m0) {
            return eerr(x, s, Memoer.popWindow(m0));
        }),
            m0 = Memoer.pushWindow(m, state.position),
            cok0 = (function(x, s, m1) {
                return cok(x, s, Memoer.popWindow(m1));
            }),
            eok0 = (function(x, s, m1) {
                return eok(x, s, Memoer.popWindow(m1));
            });
        return new(Tail)(p3.run, state, m0, cok0, peerr, eok0, peerr);
    }));
}));
(look = (function(p3) {
    return chain(getParserState, (function(v1) {
        return chain(p3, (function(v2) {
            return next(setParserState(v1), of(v2));
        }));
    }));
}));
(lookahead = (function(p3) {
    return chain(getInput, (function(v1) {
        return chain(getPosition, (function(v2) {
            return chain(p3, (function(x) {
                return sequence(new(Parser)((function(state, m, _, _0, eok, _1) {
                    var newState = state.setPosition(v2);
                    return eok(newState, newState, m);
                })), setInput(v1), of(x));
            }));
        }));
    }));
}));
(next = (function(p3, q) {
    return chain(p3, (function() {
        return q;
    }));
}));
(sequences = reduceRight.bind(null, (function(x, y) {
    return chain(y, (function() {
        return x;
    }));
})));
var x = stream.from;
(sequencea = (function(z) {
    return sequences(x(z));
}));
(sequence = (function() {
    var args0 = arguments;
    return sequencea(args.apply(null, args0));
}));
var e = (undefined ? ParseError : UnknownError);
(Parser.prototype.empty = chain(getPosition, (function(z) {
    var x0 = new(e)(z, undefined);
    return new(Parser)((function(state, m, _, _0, _1, eerr) {
        return eerr(x0, state, m);
    }));
})));
(Parser.empty = Parser.prototype.empty);
(empty = Parser.empty);
(Parser.concat = (function(p3, q) {
    return new(Parser)((function(state, m, cok, cerr, eok, eerr) {
        var position = state["position"],
            peerr = (function(errFromP, _, mFromP) {
                var qeerr = (function(errFromQ, _0, mFromQ) {
                    return eerr(new(MultipleError)(position, [errFromP, errFromQ]), state,
                        mFromQ);
                });
                return new(Tail)(q.run, state, mFromP, cok, cerr, eok, qeerr);
            });
        return new(Tail)(p3.run, state, m, cok, cerr, eok, peerr);
    }));
}));
(concat = Parser.concat);
(either = concat);
(Parser.prototype.concat = (function(p3) {
    var self = this;
    return concat(self, p3);
}));
var x0;
(choices = foldr.bind(null, (function(x0, y) {
    return new(Parser)((function(state, m, cok, cerr, eok, eerr) {
        var position = state["position"],
            peerr = (function(errFromP, _, mFromP) {
                var qeerr = (function(errFromQ, _0, mFromQ) {
                    return eerr(new(ChoiceError)(position, errFromP, errFromQ), state,
                        mFromQ);
                });
                return new(Tail)(x0.run, state, mFromP, cok, cerr, eok, qeerr);
            });
        return new(Tail)(y.run, state, m, cok, cerr, eok, peerr);
    }));
}), ((x0 = new(MultipleError)(null, [])), new(Parser)((function(state, m, _, _0, _1, eerr) {
    return eerr(x0, state, m);
})))));
var x1 = stream.from;
(choicea = (function(z) {
    return choices(x1(z));
}));
(choice = (function() {
    var args0 = arguments;
    return choicea(args.apply(null, args0));
}));
(optional = (function(x2, p3) {
    return (p3 ? concat(p3, of(x2)) : concat(x2, of(null)));
}));
(expected = (function(expect, p3) {
    return new(Parser)((function(state, m, cok, cerr, eok, eerr) {
        var eerr0 = (function(x2, state0, m0) {
            return eerr(new(ExpectError)(state0.position, expect), state0, m0);
        });
        return new(Tail)(p3.run, state, m, cok, cerr, eok, eerr0);
    }));
}));
(not = (function(p3) {
    var p4 = concat(chain(new(Parser)((function(state, m, cok, cerr, eok, eerr) {
        var peerr = (function(x2, s, m0) {
            return eerr(x2, s, Memoer.popWindow(m0));
        }),
            m0 = Memoer.pushWindow(m, state.position),
            cok0 = (function(x2, s, m1) {
                return cok(x2, s, Memoer.popWindow(m1));
            }),
            eok0 = (function(x2, s, m1) {
                return eok(x2, s, Memoer.popWindow(m1));
            });
        return new(Tail)(p3.run, state, m0, cok0, peerr, eok0, peerr);
    })), (function(x2) {
        return chain(getPosition, (function(z) {
            var x3 = new(UnexpectError)(z, x2);
            return new(Parser)((function(state, m, _, _0, _1, eerr) {
                return eerr(x3, state, m);
            }));
        }));
    })), of(null));
    return new(Parser)((function(state, m, cok, cerr, eok, eerr) {
        var peerr = (function(x2, s, m0) {
            return eerr(x2, s, Memoer.popWindow(m0));
        }),
            m0 = Memoer.pushWindow(m, state.position),
            cok0 = (function(x2, s, m1) {
                return cok(x2, s, Memoer.popWindow(m1));
            }),
            eok0 = (function(x2, s, m1) {
                return eok(x2, s, Memoer.popWindow(m1));
            });
        return new(Tail)(p4.run, state, m0, cok0, peerr, eok0, peerr);
    }));
}));
(eager = map.bind(null, stream.toArray));
(binds = (function(p3, f) {
    return chain(eager(p3), (function(x2) {
        return f.apply(undefined, x2);
    }));
}));
var f = stream.cons;
(cons = (function(p10, p20) {
    return chain(p10, (function(x2) {
        return map((function(y) {
            return f(x2, y);
        }), p20);
    }));
}));
var f0 = stream.append;
(append = (function(p10, p20) {
    return chain(p10, (function(x2) {
        return map((function(y) {
            return f0(x2, y);
        }), p20);
    }));
}));
(enumerations = foldr.bind(null, (function(x2, y) {
    return cons(y, x2);
}), of(NIL)));
var x2 = stream.from;
(enumerationa = (function(z) {
    return enumerations(x2(z));
}));
(enumeration = (function() {
    var args0 = arguments;
    return enumerationa(args.apply(null, args0));
}));
var err = new(ParserError)("Many parser applied to parser that accepts an empty string"),
    manyError = (function() {
        throw err;
    });
(many = (function(p3) {
    var safeP = new(Parser)((function(state, m, cok, cerr, eok, eerr) {
        return new(Tail)(p3.run, state, m, cok, cerr, manyError, eerr);
    }));
    return rec((function(self) {
        var p4 = cons(safeP, self);
        return (p4 ? concat(p4, of(NIL)) : concat(NIL, of(null)));
    }));
}));
(many1 = (function(p3) {
    return cons(p3, many(p3));
}));
(manyTill = (function(p3, end) {
    return rec((function(self) {
        var p4, p5;
        return concat(((p4 = chain(getInput, (function(v1) {
                return chain(getPosition, (function(v2) {
                    return chain(end, (function(x3) {
                        return sequence(new(Parser)((function(state, m,
                            _, _0, eok, _1) {
                            var newState = state.setPosition(
                                v2);
                            return eok(newState, newState,
                                m);
                        })), setInput(v1), of(x3));
                    }));
                }));
            }))), new(Parser)((function(state, m, cok, cerr, eok, eerr) {
                var peerr = (function(x3, s, m0) {
                    return eerr(x3, s, Memoer.popWindow(m0));
                }),
                    m0 = Memoer.pushWindow(m, state.position),
                    cok0 = (function(x3, s, m1) {
                        return cok(x3, s, Memoer.popWindow(m1));
                    }),
                    eok0 = (function(x3, s, m1) {
                        return eok(x3, s, Memoer.popWindow(m1));
                    });
                return new(Tail)(p4.run, state, m0, cok0, peerr, eok0, peerr);
            })))
            .map((function(_) {
                return NIL;
            })), ((p5 = cons(p3, self)), (p5 ? concat(p5, of(NIL)) : concat(NIL, of(null)))));
    }));
}));
(memo = (function(p3) {
    return new(Parser)((function(state, m, cok, cerr, eok, eerr) {
        var position = state["position"],
            key = ({
                id: p3,
                state: state
            }),
            entry = Memoer.lookup(m, position, key);
        if (entry) {
            var type = entry[0],
                x3 = entry[1],
                s = entry[2];
            switch (type) {
                case "cok":
                    return cok(x3, s, m);
                case "ceerr":
                    return cerr(x3, s, m);
                case "eok":
                    return eok(x3, s, m);
                case "eerr":
                    return eerr(x3, s, m);
            }
        }
        var cok0 = (function(x4, pstate, pm) {
            return cok(x4, pstate, Memoer.update(pm, position, key, ["cok", x4, pstate]));
        }),
            cerr0 = (function(x4, pstate, pm) {
                return cerr(x4, pstate, Memoer.update(pm, position, key, ["cerr", x4, pstate]));
            }),
            eok0 = (function(x4, pstate, pm) {
                return eok(x4, pstate, Memoer.update(pm, position, key, ["eok", x4, pstate]));
            }),
            eerr0 = (function(x4, pstate, pm) {
                return eerr(x4, pstate, Memoer.update(pm, position, key, ["eerr", x4, pstate]));
            });
        return new(Tail)(p3.run, state, m, cok0, cerr0, eok0, eerr0);
    }));
}));
var defaultErr = (function(pos, tok) {
    return new(UnexpectError)(pos, ((tok === null) ? "end of input" : tok));
});
(token = (function(consume, onErr) {
    var errorHandler = (onErr || defaultErr);
    return new(Parser)((function(s, m, cok, cerr, eok, eerr) {
        var tok, pcok, p3;
        return (s.isEmpty() ? eerr(errorHandler(s.position, null), s, m) : ((tok = s.first()), (consume(
            tok) ? ((pcok = (function(x3, s0, m0) {
            return cok(x3, s0, Memoer.prune(m0, s0.position));
        })), (p3 = s.next(tok)), new(Tail)(p3.run, s, m, pcok, cerr, pcok, cerr)) : eerr(
            errorHandler(s.position, tok), s, m))));
    }));
}));
var p3 = token((function() {
    return true;
}));
(anyToken = (p3.run.hasOwnProperty("displayName") ? label("Any Token", new(Parser)((function(state, m, cok, cerr, eok,
    eerr) {
    return new(Tail)(p3.run, state, m, cok, cerr, eok, eerr);
}))) : new(Parser)(Object.defineProperty(p3.run, "displayName", ({
    value: "Any Token",
    writable: false
})))));
var p4 = concat(chain(new(Parser)((function(state, m, cok, cerr, eok, eerr) {
    var peerr = (function(x3, s, m0) {
        return eerr(x3, s, Memoer.popWindow(m0));
    }),
        m0 = Memoer.pushWindow(m, state.position),
        cok0 = (function(x3, s, m1) {
            return cok(x3, s, Memoer.popWindow(m1));
        }),
        eok0 = (function(x3, s, m1) {
            return eok(x3, s, Memoer.popWindow(m1));
        });
    return new(Tail)(anyToken.run, state, m0, cok0, peerr, eok0, peerr);
})), (function(x3) {
    return chain(getPosition, (function(z) {
        var x4 = new(UnexpectError)(z, x3);
        return new(Parser)((function(state, m, _, _0, _1, eerr) {
            return eerr(x4, state, m);
        }));
    }));
})), of(null)),
    p5 = new(Parser)((function(state, m, cok, cerr, eok, eerr) {
        var peerr = (function(x3, s, m0) {
            return eerr(x3, s, Memoer.popWindow(m0));
        }),
            m0 = Memoer.pushWindow(m, state.position),
            cok0 = (function(x3, s, m1) {
                return cok(x3, s, Memoer.popWindow(m1));
            }),
            eok0 = (function(x3, s, m1) {
                return eok(x3, s, Memoer.popWindow(m1));
            });
        return new(Tail)(p4.run, state, m0, cok0, peerr, eok0, peerr);
    }));
(eof = (p5.run.hasOwnProperty("displayName") ? label("EOF", new(Parser)((function(state, m, cok, cerr, eok, eerr) {
    return new(Tail)(p5.run, state, m, cok, cerr, eok, eerr);
}))) : new(Parser)(Object.defineProperty(p5.run, "displayName", ({
    value: "EOF",
    writable: false
})))));
(exec = (function() {
    var args0 = arguments;
    return trampoline(unparser.apply(null, args0));
}));
(parseState = (function(p6, state, ok, err0) {
    return exec(p6, state, Memoer.empty, ok, err0, ok, err0);
}));
(parseStream = (function(p6, s, ud, ok, err0) {
    var state = new(ParserState)(s, Position.initial, ud);
    return exec(p6, state, Memoer.empty, ok, err0, ok, err0);
}));
(parse = (function(p6, input, ud, ok, err0) {
    var s = stream.from(input),
        state = new(ParserState)(s, Position.initial, ud);
    return exec(p6, state, Memoer.empty, ok, err0, ok, err0);
}));
var err0 = (function(x3) {
    throw x3;
});
(runState = (function(p6, state) {
    return exec(p6, state, Memoer.empty, identity, err0, identity, err0);
}));
(runStream = (function(p6, s, ud) {
    return runState(p6, new(ParserState)(s, Position.initial, ud));
}));
(run = (function(p6, input, ud) {
    var s = stream.from(input);
    return runState(p6, new(ParserState)(s, Position.initial, ud));
}));
var ok = (function() {
    return true;
}),
    err1 = (function() {
        return false;
    });
(testState = (function(p6, state) {
    return exec(p6, state, Memoer.empty, ok, err1, ok, err1);
}));
(testStream = (function(p6, s, ud) {
    return testState(p6, new(ParserState)(s, Position.initial, ud));
}));
(test = (function(p6, input, ud) {
    var s = stream.from(input);
    return testState(p6, new(ParserState)(s, Position.initial, ud));
}));
(exports["Tail"] = Tail);
(exports["trampoline"] = trampoline);
(exports["ParserError"] = ParserError);
(exports["ParseError"] = ParseError);
(exports["MultipleError"] = MultipleError);
(exports["UnknownError"] = UnknownError);
(exports["UnexpectError"] = UnexpectError);
(exports["ExpectError"] = ExpectError);
(exports["ParserState"] = ParserState);
(exports["Position"] = Position);
(exports["Parser"] = Parser);
(exports["label"] = label);
(exports["late"] = late);
(exports["rec"] = rec);
(exports["unparser"] = unparser);
(exports["always"] = always);
(exports["of"] = of);
(exports["never"] = never);
(exports["bind"] = bind);
(exports["chain"] = chain);
(exports["map"] = map);
(exports["ap"] = ap);
(exports["extract"] = extract);
(exports["getParserState"] = getParserState);
(exports["setParserState"] = setParserState);
(exports["modifyParserState"] = modifyParserState);
(exports["getState"] = getState);
(exports["setState"] = setState);
(exports["modifyState"] = modifyState);
(exports["getInput"] = getInput);
(exports["setInput"] = setInput);
(exports["getPosition"] = getPosition);
(exports["setPosition"] = setPosition);
(exports["fail"] = fail);
(exports["attempt"] = attempt);
(exports["look"] = look);
(exports["lookahead"] = lookahead);
(exports["next"] = next);
(exports["sequences"] = sequences);
(exports["sequencea"] = sequencea);
(exports["sequence"] = sequence);
(exports["empty"] = empty);
(exports["either"] = either);
(exports["concat"] = concat);
(exports["choices"] = choices);
(exports["choicea"] = choicea);
(exports["choice"] = choice);
(exports["optional"] = optional);
(exports["expected"] = expected);
(exports["not"] = not);
(exports["eager"] = eager);
(exports["binds"] = binds);
(exports["cons"] = cons);
(exports["append"] = append);
(exports["enumerations"] = enumerations);
(exports["enumerationa"] = enumerationa);
(exports["enumeration"] = enumeration);
(exports["many"] = many);
(exports["many1"] = many1);
(exports["manyTill"] = manyTill);
(exports["memo"] = memo);
(exports["token"] = token);
(exports["anyToken"] = anyToken);
(exports["eof"] = eof);
(exports["exec"] = exec);
(exports["parseState"] = parseState);
(exports["parseStream"] = parseStream);
(exports["parse"] = parse);
(exports["runState"] = runState);
(exports["runStream"] = runStream);
(exports["run"] = run);
(exports["testState"] = testState);
(exports["testStream"] = testStream);
(exports["test"] = test);
},{"nu-stream":"node_modules/nu-stream/index.js","seshet":"node_modules/seshet/dist_node/seshet.js"}],"node_modules/bennu/dist_node/incremental.js":[function(require,module,exports) {
/*
 * THIS FILE IS AUTO GENERATED FROM 'lib/incremental.kep'
 * DO NOT EDIT
*/
"use strict";
var __o = require("./parse"),
    stream = require("nu-stream")["stream"],
    provide, provideString, finish, parseIncState, parseInc, runIncState, runInc, runManyState, runManyStream, runMany,
        bind = __o["bind"],
    getParserState = __o["getParserState"],
    next = __o["next"],
    optional = __o["optional"],
    parseState = __o["parseState"],
    Parser = __o["Parser"],
    ParserState = __o["ParserState"],
    Position = __o["Position"],
    runState = __o["runState"],
    trampoline = __o["trampoline"],
    streamFrom = stream["from"],
    isEmpty = stream["isEmpty"],
    NIL = stream["NIL"],
    memoStream = stream["memoStream"],
    Request = (function(chunk, k) {
        var self = this;
        (self.chunk = chunk);
        (self.k = k);
    }),
    Session = (function(done, k, chunks) {
        var self = this;
        (self.done = done);
        (self.k = k);
        (self.chunks = chunks);
    });
(Session.prototype.addChunk = (function(c) {
    var self = this;
    return new(Session)(self.done, self.k, self.chunks.concat(c));
}));
(Session.prototype.hasChunk = (function(c) {
    var self = this;
    return (c < self.chunks.length);
}));
(Session.prototype.getChunk = (function(c) {
    var self = this;
    return self.chunks[c];
}));
var IncrementalState = (function(chunk, state) {
    var self = this;
    (self.chunk = chunk);
    (self.state = state);
});
Object.defineProperties(IncrementalState.prototype, ({
    input: ({
        get: (function() {
            var self = this;
            return self.state.input;
        })
    }),
    position: ({
        get: (function() {
            var self = this;
            return self.state.position;
        })
    }),
    userState: ({
        get: (function() {
            var self = this;
            return self.state.userState;
        })
    })
}));
(IncrementalState.prototype.eq = (function(other) {
    var self = this;
    return ((other && (other.chunk === self.chunk)) && self.state.eq(other.state));
}));
(IncrementalState.prototype.isEmpty = (function() {
    var self = this;
    return self.state.isEmpty();
}));
(IncrementalState.prototype.first = (function() {
    var self = this;
    return self.state.first();
}));
(IncrementalState.prototype.next = (function(x) {
    var self = this;
    if ((!self._next)) {
        var chunk = self.chunk;
        (self._next = bind(next(self.state.next(x), getParserState), (function(innerState) {
            var state;
            return (innerState.isEmpty() ? new(Parser)((function(_, m, cok) {
                return new(Request)((chunk + 1), (function(i) {
                    return cok(x, new(IncrementalState)((chunk + 1), innerState
                        .setInput(i)), m);
                }));
            })) : ((state = new(IncrementalState)(chunk, innerState)), new(Parser)((function(_,
                m, cok) {
                return cok(x, state, m);
            }))));
        })));
    }
    return self._next;
}));
(IncrementalState.prototype.setInput = (function(input) {
    var self = this;
    return new(IncrementalState)(self.chunk, self.state.setInput(input));
}));
(IncrementalState.prototype.setPosition = (function(position) {
    var self = this;
    return new(IncrementalState)(self.chunk, self.state.setPosition(position));
}));
(IncrementalState.prototype.setUserState = (function(userState) {
    var self = this;
    return new(IncrementalState)(self.chunk, self.state.setUserState(userState));
}));
var forceProvide = (function(c, r) {
    if (r.done) return r;
    var r2 = r.addChunk(c),
        result = trampoline(r2.k(c));
    while (((result instanceof Request) && r2.hasChunk(result.chunk))) {
        (result = trampoline(result.k(r2.getChunk(result.chunk))));
    }
    return ((result instanceof Request) ? new(Session)(false, result.k, r2.chunks) : result);
});
(provide = (function(c, r) {
    return (isEmpty(c) ? r : forceProvide(c, r));
}));
(provideString = (function(input, r) {
    return provide(streamFrom(input), r);
}));
var x = forceProvide.bind(null, NIL);
(finish = (function(z) {
    var r = x(z);
    return r.k();
}));
(parseIncState = (function(p, state, ok, err) {
    var pok = (function(x0, s) {
        return new(Session)(true, ok.bind(null, x0, s));
    }),
        perr = (function(x0, s) {
            return new(Session)(true, err.bind(null, x0, s));
        });
    return provide(state.input, new(Session)(false, (function(i) {
        return parseState(p, new(IncrementalState)(0, state.setInput(i)), pok, perr);
    }), []));
}));
(parseInc = (function(p, ud, ok, err) {
    return parseIncState(p, new(ParserState)(NIL, Position.initial, ud), ok, err);
}));
var ok = (function(x0) {
    return x0;
}),
    err = (function(x0) {
        throw x0;
    });
(runIncState = (function(p, state) {
    return parseIncState(p, state, ok, err);
}));
(runInc = (function(p, ud) {
    return runIncState(p, new(ParserState)(NIL, Position.initial, ud));
}));
(runManyState = (function(p, state) {
    var manyP = optional(NIL, bind(p, (function(x0) {
        return new(Parser)((function(state0, m, _, _0, eok, _1) {
            return eok(memoStream(x0, runState.bind(null, manyP, state0, m)), state0, m);
        }));
    })));
    return runState(manyP, state);
}));
(runManyStream = (function(p, s, ud) {
    return runManyState(p, new(ParserState)(s, Position.initial, ud));
}));
(runMany = (function(p, input, ud) {
    return runManyStream(p, streamFrom(input), ud);
}));
(exports["provide"] = provide);
(exports["provideString"] = provideString);
(exports["finish"] = finish);
(exports["parseIncState"] = parseIncState);
(exports["parseInc"] = parseInc);
(exports["runIncState"] = runIncState);
(exports["runInc"] = runInc);
(exports["runManyState"] = runManyState);
(exports["runManyStream"] = runManyStream);
(exports["runMany"] = runMany);
},{"./parse":"node_modules/bennu/dist_node/parse.js","nu-stream":"node_modules/nu-stream/index.js"}],"node_modules/bennu/dist_node/lang.js":[function(require,module,exports) {
/*
 * THIS FILE IS AUTO GENERATED FROM 'lib/lang.kep'
 * DO NOT EDIT
*/
"use strict";
var __o = require("nu-stream")["stream"],
    __o0 = require("nu-stream")["gen"],
    __o1 = require("./parse"),
    times, atMostTimes, betweenTimes, then, between, sepBy1, sepBy, sepEndBy1, sepEndBy, endBy1, endBy, chainl1, chainl,
        chainr1, chainr, NIL = __o["NIL"],
    repeat = __o0["repeat"],
    append = __o1["append"],
    always = __o1["always"],
    bind = __o1["bind"],
    cons = __o1["cons"],
    either = __o1["either"],
    enumerations = __o1["enumerations"],
    late = __o1["late"],
    many = __o1["many"],
    many1 = __o1["many1"],
    next = __o1["next"],
    optional = __o1["optional"],
    rec = __o1["rec"],
    _end = always(NIL),
    _optionalValueParser = optional.bind(null, NIL);
(times = (function() {
    var args = arguments;
    return enumerations(repeat.apply(null, args));
}));
(atMostTimes = (function(n, p) {
    return ((n <= 0) ? _end : _optionalValueParser(cons(p, late((function() {
        return atMostTimes((n - 1), p);
    })))));
}));
(betweenTimes = (function(min, max, p) {
    var args, n;
    return append(((args = [min, p]), enumerations(repeat.apply(null, args))), ((n = (max - min)), ((n <= 0) ?
        _end : _optionalValueParser(cons(p, late((function() {
            return atMostTimes((n - 1), p);
        })))))));
}));
(then = (function(p, q) {
    return bind(p, (function(x) {
        return next(q, always(x));
    }));
}));
(between = (function(open, close, p) {
    return next(open, bind(p, (function(x) {
        return next(close, always(x));
    })));
}));
(sepBy1 = (function(sep, p) {
    return cons(p, many(next(sep, p)));
}));
(sepBy = (function() {
    var args = arguments;
    return _optionalValueParser(sepBy1.apply(null, args));
}));
(sepEndBy1 = (function(sep, p) {
    return rec((function(self) {
        return cons(p, _optionalValueParser(next(sep, _optionalValueParser(self))));
    }));
}));
(sepEndBy = (function(sep, p) {
    return either(rec((function(self) {
        return cons(p, _optionalValueParser(next(sep, _optionalValueParser(self))));
    })), next(optional(sep), _end));
}));
(endBy1 = (function(sep, p) {
    return many1(bind(p, (function(x) {
        return next(sep, always(x));
    })));
}));
(endBy = (function(sep, p) {
    return many(bind(p, (function(x) {
        return next(sep, always(x));
    })));
}));
(chainl1 = (function(op, p) {
    return bind(p, (function chain(x) {
        return optional(x, bind(op, (function(f) {
            return bind(p, (function(y) {
                return chain(f(x, y));
            }));
        })));
    }));
}));
(chainl = (function(op, x, p) {
    return optional(x, bind(p, (function chain(x0) {
        return optional(x0, bind(op, (function(f) {
            return bind(p, (function(y) {
                return chain(f(x0, y));
            }));
        })));
    })));
}));
(chainr1 = (function(op, p) {
    return rec((function(self) {
        return bind(p, (function(x) {
            return optional(x, bind(op, (function(f) {
                return self.map((function(y) {
                    return f(x, y);
                }));
            })));
        }));
    }));
}));
(chainr = (function(op, x, p) {
    return optional(x, rec((function(self) {
        return bind(p, (function(x0) {
            return optional(x0, bind(op, (function(f) {
                return self.map((function(y) {
                    return f(x0, y);
                }));
            })));
        }));
    })));
}));
(exports["times"] = times);
(exports["atMostTimes"] = atMostTimes);
(exports["betweenTimes"] = betweenTimes);
(exports["then"] = then);
(exports["between"] = between);
(exports["sepBy1"] = sepBy1);
(exports["sepBy"] = sepBy);
(exports["sepEndBy1"] = sepEndBy1);
(exports["sepEndBy"] = sepEndBy);
(exports["endBy1"] = endBy1);
(exports["endBy"] = endBy);
(exports["chainl1"] = chainl1);
(exports["chainl"] = chainl);
(exports["chainr1"] = chainr1);
(exports["chainr"] = chainr);
},{"nu-stream":"node_modules/nu-stream/index.js","./parse":"node_modules/bennu/dist_node/parse.js"}],"node_modules/bennu/dist_node/text.js":[function(require,module,exports) {
/*
 * THIS FILE IS AUTO GENERATED FROM 'lib/text.kep'
 * DO NOT EDIT
*/
"use strict";
var __o = require("./parse"),
    character, oneOf, noneOf, string, trie, match, anyChar, letter, space, digit, always = __o["always"],
    attempt = __o["attempt"],
    bind = __o["bind"],
    optional = __o["optional"],
    ExpectError = __o["ExpectError"],
    next = __o["next"],
    label = __o["label"],
    token = __o["token"],
    join = Function.prototype.call.bind(Array.prototype.join),
    map = Function.prototype.call.bind(Array.prototype.map),
    reduce = Function.prototype.call.bind(Array.prototype.reduce),
    reduceRight = Function.prototype.call.bind(Array.prototype.reduceRight),
    StringError = (function(position, string, index, expected, found) {
        var self = this;
        ExpectError.call(self, position, expected, found);
        (self.string = string);
        (self.index = index);
    });
(StringError.prototype = new(ExpectError)());
(StringError.prototype.constructor = StringError);
Object.defineProperty(StringError.prototype, "errorMessage", ({
    "get": (function() {
        var self = this;
        return ((((((("In string: '" + self.string) + "' at index: ") + self.index) + ", Expected: ") +
            self.expected) + " Found: ") + (self.found ? self.found : "end of input"));
    })
}));
var unbox = (function(y) {
    return ("" + y);
}),
    _character = (function(c, err) {
        var x;
        return token(((x = ("" + c)), (function(r) {
            return (x === ("" + r));
        })), err);
    });
(character = (function(c) {
    return _character(c, (function(pos, tok) {
        return new(ExpectError)(pos, c, ((tok === null) ? "end of input" : tok));
    }));
}));
(oneOf = (function(chars) {
    var chars0 = map(chars, unbox),
        msg;
    return token((function(x) {
        return (chars0.indexOf(("" + x)) >= 0);
    }), ((msg = join(chars0, " or ")), (function(pos, tok) {
        return new(ExpectError)(pos, msg, ((tok === null) ? "end of input" : tok));
    })));
}));
(noneOf = (function(chars) {
    var chars0 = map(chars, unbox),
        msg;
    return token((function(z) {
        var x = (chars0.indexOf(("" + z)) >= 0);
        return (!x);
    }), ((msg = ("none of:" + join(chars0, " or "))), (function(pos, tok) {
        return new(ExpectError)(pos, msg, ((tok === null) ? "end of input" : tok));
    })));
}));
var reducer = (function(p, c, i, s) {
    return next(_character(c, (function(pos, tok) {
        return new(StringError)(pos, s, i, c, tok);
    })), p);
});
(string = (function(s) {
    return attempt(reduceRight(s, reducer, always(("" + s))));
}));
var wordReduce = (function(parent, l) {
    (parent[l] = (parent[l] || ({})));
    return parent[l];
}),
    wordsReduce = (function(trie, word) {
        var node = reduce(word, wordReduce, trie);
        (node[""] = word);
        return trie;
    }),
    _trie = (function(trie) {
        var chars, msg, keys = Object.keys(trie),
            paths = reduce(keys, (function(p, c) {
                if (c) {
                    (p[c] = _trie(trie[c]));
                }
                return p;
            }), ({})),
            select = attempt(bind(((chars = map(keys, unbox)), token((function(x) {
                return (chars.indexOf(("" + x)) >= 0);
            }), ((msg = join(chars, " or ")), (function(pos, tok) {
                return new(ExpectError)(pos, msg, ((tok === null) ? "end of input" : tok));
            })))), (function(y) {
                return paths[y];
            })));
        return (trie.hasOwnProperty("") ? optional(trie[""], select) : select);
    });
(trie = (function(z) {
    var z0 = reduce(z, wordsReduce, ({})),
        chars, msg, keys, paths, select;
    return attempt(((keys = Object.keys(z0)), (paths = reduce(keys, (function(p, c) {
        if (c) {
            (p[c] = _trie(z0[c]));
        }
        return p;
    }), ({}))), (select = attempt(bind(((chars = map(keys, unbox)), token((function(x) {
        return (chars.indexOf(("" + x)) >= 0);
    }), ((msg = join(chars, " or ")), (function(pos, tok) {
        return new(ExpectError)(pos, msg, ((tok === null) ? "end of input" :
            tok));
    })))), (function(y) {
        return paths[y];
    })))), (z0.hasOwnProperty("") ? optional(z0[""], select) : select)));
}));
(match = (function(pattern, expected) {
    return token(RegExp.prototype.test.bind(pattern), (function(pos, tok) {
        return new(ExpectError)(pos, expected, ((tok === null) ? "end of input" : tok));
    }));
}));
var pattern;
(anyChar = label("Any Character", ((pattern = /^.$/), token(RegExp.prototype.test.bind(pattern), (function(pos, tok) {
    return new(ExpectError)(pos, "any character", ((tok === null) ? "end of input" : tok));
})))));
var pattern0;
(letter = label("Any Letter", ((pattern0 = /^[a-z]$/i), token(RegExp.prototype.test.bind(pattern0), (function(pos, tok) {
    return new(ExpectError)(pos, "any letter character", ((tok === null) ? "end of input" : tok));
})))));
var pattern1;
(space = label("Any Whitespace", ((pattern1 = /^\s$/i), token(RegExp.prototype.test.bind(pattern1), (function(pos, tok) {
    return new(ExpectError)(pos, "any space character", ((tok === null) ? "end of input" : tok));
})))));
var pattern2;
(digit = label("Any Digit", ((pattern2 = /^[0-9]$/i), token(RegExp.prototype.test.bind(pattern2), (function(pos, tok) {
    return new(ExpectError)(pos, "any digit character", ((tok === null) ? "end of input" : tok));
})))));
(exports["character"] = character);
(exports["oneOf"] = oneOf);
(exports["noneOf"] = noneOf);
(exports["string"] = string);
(exports["trie"] = trie);
(exports["match"] = match);
(exports["anyChar"] = anyChar);
(exports["letter"] = letter);
(exports["space"] = space);
(exports["digit"] = digit);
},{"./parse":"node_modules/bennu/dist_node/parse.js"}],"node_modules/bennu/index.js":[function(require,module,exports) {
module.exports = {
    'parse': require('./dist_node/parse'),
    'incremental': require('./dist_node/incremental'),
    'lang': require('./dist_node/lang'),
    'text': require('./dist_node/text')
};
},{"./dist_node/parse":"node_modules/bennu/dist_node/parse.js","./dist_node/incremental":"node_modules/bennu/dist_node/incremental.js","./dist_node/lang":"node_modules/bennu/dist_node/lang.js","./dist_node/text":"node_modules/bennu/dist_node/text.js"}],"index.js":[function(require,module,exports) {
"use strict";

require("./style.css");

var _bennu = require("bennu");

var _nuStream = require("nu-stream");

// Import stylesheets
var renderJson = window.renderjson.set_icons("+", "-").set_show_to_level(10).set_max_string_length(100);

function p(x) {
  //console.log(...arguments);
  return _bennu.parse.always(x);
}

function toString(x) {
  return _nuStream.stream.foldl(function (a, b) {
    return a + b;
  }, "", x);
}

function parseNumber(x) {
  var r = _nuStream.stream.foldl(function (a, b) {
    return a + b;
  }, "", x);

  return r.replace(/\D/g, '');
} // Write Javascript code!


var appDiv = document.getElementById('app');
appDiv.innerHTML = "<h1>JS Starter</h1>";
var oneSpace = _bennu.text.space;

var unlimitedSpace = _bennu.parse.many(_bennu.text.space);

var assignOperator = _bennu.text.character('=');

var endCommand = _bennu.text.character(';');

var navigateOperator = _bennu.text.character('.');

var openObject = _bennu.text.character('{');

var closeObject = _bennu.text.character('}');

var autoKeyword = _bennu.text.string('auto');

var letterDigit = _bennu.parse.either(_bennu.text.letter, _bennu.text.digit);

var identifier = _bennu.parse.bind(_bennu.parse.many(letterDigit), function (x) {
  return p({
    type: "Identifier",
    name: toString(x)
  });
});

var propertyAcessor = _bennu.parse.bind(_bennu.parse.next(navigateOperator, identifier), function (a, b, c) {
  return p({
    type: "Property",
    id: a
  });
});

var numberValue = _bennu.parse.bind(_bennu.parse.manyTill(_bennu.text.digit, oneSpace), function (x) {
  return p({
    type: "NumberConstant",
    value: parseNumber(x)
  });
});

var stringBegin = _bennu.text.character('"');

var stringEnd = _bennu.text.character('"');

var stringContent = _bennu.parse.manyTill(_bennu.text.anyChar, stringEnd);

var stringValue = _bennu.parse.bind(_bennu.lang.between(stringBegin, stringEnd, stringContent), function (x) {
  return p({
    type: "StringConstant",
    value: toString(x)
  });
});

var constantValue = _bennu.parse.either(stringValue, numberValue);

var assignProperty = _bennu.parse.binds(_bennu.parse.enumeration(propertyAcessor, unlimitedSpace, assignOperator, unlimitedSpace, constantValue, endCommand, unlimitedSpace), function (a, b, c, d, e, f, g) {
  return p({
    type: "AssignmentExpression",
    id: a,
    init: e
  });
});

var expressionList = _bennu.parse.eager(_bennu.parse.many(assignProperty));

var objectInstance = _bennu.parse.binds(_bennu.parse.enumeration(_bennu.parse.optional(_bennu.lang.then(identifier, unlimitedSpace)), _bennu.lang.between(_bennu.parse.next(openObject, unlimitedSpace), closeObject, expressionList)), function (a, b, c, d) {
  return p({
    type: "Object",
    bodyType: a,
    body: b
  });
});

var initVariable = _bennu.parse.binds(_bennu.parse.enumeration(autoKeyword, unlimitedSpace, identifier, unlimitedSpace, assignOperator, unlimitedSpace, objectInstance), function (a, b, c, d, e, f, g) {
  return p({
    type: "InitVariable",
    varType: "auto",
    id: c,
    init: g
  }, 'initVariable');
});

var typeKeyword = _bennu.text.string("type");

var fieldDeclaration = _bennu.parse.binds(_bennu.parse.enumeration(identifier, unlimitedSpace, identifier, endCommand, unlimitedSpace), function (a, b, c, d) {
  return p({
    type: "FieldDeclaration",
    typeName: a,
    name: c
  });
});

var fieldList = _bennu.parse.manyTill(fieldDeclaration, closeObject);

var typeDeclaration = _bennu.parse.binds(_bennu.parse.enumeration(typeKeyword, unlimitedSpace, identifier, unlimitedSpace, _bennu.parse.eager(_bennu.lang.between(_bennu.parse.next(openObject, unlimitedSpace), closeObject, _bennu.parse.many(fieldDeclaration))), endCommand), function (a, b, c, d, e, f, g, h) {
  return p({
    type: "TypeDeclaration",
    id: c,
    fields: e
  }, 'typeDeclaration');
});

var functionCallArgument = _bennu.parse.choice(initVariable, identifier);

var functionCall = _bennu.parse.binds(_bennu.parse.enumeration(identifier, _bennu.text.character('('), _bennu.parse.eager(_bennu.lang.sepBy(_bennu.text.character(','), functionCallArgument)), _bennu.text.character(')')), function (a, b, c) {
  return p({
    type: "FunctionCall",
    name: a,
    arguments: c
  });
});

var expression = _bennu.parse.choice(_bennu.lang.then(initVariable, endCommand), _bennu.lang.then(functionCall, endCommand));

var start = _bennu.parse.eager(_bennu.lang.sepEndBy(unlimitedSpace, _bennu.parse.choice(typeDeclaration, expression)));

function toJS(AST) {
  var code = [];
  var current = AST;

  if (Array.isArray(current)) {
    current.forEach(function (x) {
      var r = toJS(x);
      code.push(r);
    });
  } else if (current.type == "TypeDeclaration") {
    code.push("function ".concat(current.id.name, " () {\n"));
    current.fields.forEach(function (x) {
      code.push("this.".concat(x.name.name, " = null;\n"));
    });
    code.push("}\n");
  } else if (current.type == "InitVariable") {
    code.push("function f1 () {\n");
    code.push("var $return = {};\n");
    var initValue = toJS(current.init);
    code.push("".concat(initValue));
    code.push("return $return;\n");
    code.push("}\n");
    code.push("var ".concat(current.id.name, " = f1();\n"));
  } else if (current.type == "Object") {
    current.body.forEach(function (x) {
      var line = toJS(x);
      code.push("".concat(line, ";\n"));
    });
  } else if (current.type == "AssignmentExpression") {
    var initValue = toJS(current.init);
    code.push("$return.".concat(current.id.id.name, " = ").concat(initValue));
  } else if (current.type == "StringConstant") {
    return "'".concat(current.value, "'");
  } else if (current.type == "NumberConstant") {
    return current.value;
  } else if (current.type == "FunctionCall") {
    var name = toJS(current.name);
    var args = [];

    for (var i = 0; i < current.arguments.length; ++i) {
      var x = current.arguments[i];
      code.push("var a".concat(i, " = ").concat(toJS(x), ";\n"));
      args.push("a".concat(i));
    }

    ;
    code.push("".concat(name, "("));
    code.push(args.join(","));
    code.push(");\n");
  } else if (current.type == "Identifier") {
    if (current.name == "print") return "console.log";
    return current.name;
  }

  return code.join("");
}

function setSelectionRange(input, selectionStart, selectionEnd) {
  if (input.setSelectionRange) {
    input.focus();
    input.setSelectionRange(selectionStart, selectionEnd);
  } else if (input.createTextRange) {
    var range = input.createTextRange();
    range.collapse(true);
    range.moveEnd('character', selectionEnd);
    range.moveStart('character', selectionStart);
    range.select();
  }
}

function parseCode() {
  var code = document.getElementById("code");

  try {
    appDiv.innerHTML = "";

    var pr = _bennu.incremental.runInc(start);

    pr = _bennu.incremental.provideString(code.value, pr);

    var r = _bennu.incremental.finish(pr);

    appDiv.append(renderJson(r));
    var code = toJS(r);
    console.log(code);
    eval(code);
  } catch (e) {
    console.error(e);
    appDiv.append(renderJson(e));
    setSelectionRange(code, e.position.index, e.position.index + 1);
  }
}

parseCode();
document.getElementById("code").addEventListener("input", parseCode);
},{"./style.css":"style.css","bennu":"node_modules/bennu/index.js","nu-stream":"node_modules/nu-stream/index.js"}],"../../../../../../../root/.nvm/versions/node/v12.4.0/lib/node_modules/parcel/src/builtins/hmr-runtime.js":[function(require,module,exports) {
var global = arguments[3];
var OVERLAY_ID = '__parcel__error__overlay__';
var OldModule = module.bundle.Module;

function Module(moduleName) {
  OldModule.call(this, moduleName);
  this.hot = {
    data: module.bundle.hotData,
    _acceptCallbacks: [],
    _disposeCallbacks: [],
    accept: function (fn) {
      this._acceptCallbacks.push(fn || function () {});
    },
    dispose: function (fn) {
      this._disposeCallbacks.push(fn);
    }
  };
  module.bundle.hotData = null;
}

module.bundle.Module = Module;
var checkedAssets, assetsToAccept;
var parent = module.bundle.parent;

if ((!parent || !parent.isParcelRequire) && typeof WebSocket !== 'undefined') {
  var hostname = "" || location.hostname;
  var protocol = location.protocol === 'https:' ? 'wss' : 'ws';
  var ws = new WebSocket(protocol + '://' + hostname + ':' + "54492" + '/');

  ws.onmessage = function (event) {
    checkedAssets = {};
    assetsToAccept = [];
    var data = JSON.parse(event.data);

    if (data.type === 'update') {
      var handled = false;
      data.assets.forEach(function (asset) {
        if (!asset.isNew) {
          var didAccept = hmrAcceptCheck(global.parcelRequire, asset.id);

          if (didAccept) {
            handled = true;
          }
        }
      }); // Enable HMR for CSS by default.

      handled = handled || data.assets.every(function (asset) {
        return asset.type === 'css' && asset.generated.js;
      });

      if (handled) {
        console.clear();
        data.assets.forEach(function (asset) {
          hmrApply(global.parcelRequire, asset);
        });
        assetsToAccept.forEach(function (v) {
          hmrAcceptRun(v[0], v[1]);
        });
      } else {
        window.location.reload();
      }
    }

    if (data.type === 'reload') {
      ws.close();

      ws.onclose = function () {
        location.reload();
      };
    }

    if (data.type === 'error-resolved') {
      console.log('[parcel] ✨ Error resolved');
      removeErrorOverlay();
    }

    if (data.type === 'error') {
      console.error('[parcel] 🚨  ' + data.error.message + '\n' + data.error.stack);
      removeErrorOverlay();
      var overlay = createErrorOverlay(data);
      document.body.appendChild(overlay);
    }
  };
}

function removeErrorOverlay() {
  var overlay = document.getElementById(OVERLAY_ID);

  if (overlay) {
    overlay.remove();
  }
}

function createErrorOverlay(data) {
  var overlay = document.createElement('div');
  overlay.id = OVERLAY_ID; // html encode message and stack trace

  var message = document.createElement('div');
  var stackTrace = document.createElement('pre');
  message.innerText = data.error.message;
  stackTrace.innerText = data.error.stack;
  overlay.innerHTML = '<div style="background: black; font-size: 16px; color: white; position: fixed; height: 100%; width: 100%; top: 0px; left: 0px; padding: 30px; opacity: 0.85; font-family: Menlo, Consolas, monospace; z-index: 9999;">' + '<span style="background: red; padding: 2px 4px; border-radius: 2px;">ERROR</span>' + '<span style="top: 2px; margin-left: 5px; position: relative;">🚨</span>' + '<div style="font-size: 18px; font-weight: bold; margin-top: 20px;">' + message.innerHTML + '</div>' + '<pre>' + stackTrace.innerHTML + '</pre>' + '</div>';
  return overlay;
}

function getParents(bundle, id) {
  var modules = bundle.modules;

  if (!modules) {
    return [];
  }

  var parents = [];
  var k, d, dep;

  for (k in modules) {
    for (d in modules[k][1]) {
      dep = modules[k][1][d];

      if (dep === id || Array.isArray(dep) && dep[dep.length - 1] === id) {
        parents.push(k);
      }
    }
  }

  if (bundle.parent) {
    parents = parents.concat(getParents(bundle.parent, id));
  }

  return parents;
}

function hmrApply(bundle, asset) {
  var modules = bundle.modules;

  if (!modules) {
    return;
  }

  if (modules[asset.id] || !bundle.parent) {
    var fn = new Function('require', 'module', 'exports', asset.generated.js);
    asset.isNew = !modules[asset.id];
    modules[asset.id] = [fn, asset.deps];
  } else if (bundle.parent) {
    hmrApply(bundle.parent, asset);
  }
}

function hmrAcceptCheck(bundle, id) {
  var modules = bundle.modules;

  if (!modules) {
    return;
  }

  if (!modules[id] && bundle.parent) {
    return hmrAcceptCheck(bundle.parent, id);
  }

  if (checkedAssets[id]) {
    return;
  }

  checkedAssets[id] = true;
  var cached = bundle.cache[id];
  assetsToAccept.push([bundle, id]);

  if (cached && cached.hot && cached.hot._acceptCallbacks.length) {
    return true;
  }

  return getParents(global.parcelRequire, id).some(function (id) {
    return hmrAcceptCheck(global.parcelRequire, id);
  });
}

function hmrAcceptRun(bundle, id) {
  var cached = bundle.cache[id];
  bundle.hotData = {};

  if (cached) {
    cached.hot.data = bundle.hotData;
  }

  if (cached && cached.hot && cached.hot._disposeCallbacks.length) {
    cached.hot._disposeCallbacks.forEach(function (cb) {
      cb(bundle.hotData);
    });
  }

  delete bundle.cache[id];
  bundle(id);
  cached = bundle.cache[id];

  if (cached && cached.hot && cached.hot._acceptCallbacks.length) {
    cached.hot._acceptCallbacks.forEach(function (cb) {
      cb();
    });

    return true;
  }
}
},{}]},{},["../../../../../../../root/.nvm/versions/node/v12.4.0/lib/node_modules/parcel/src/builtins/hmr-runtime.js","index.js"], null)
//# sourceMappingURL=/milkLanguage.e31bb0bc.js.map