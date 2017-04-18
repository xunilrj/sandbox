var Context = function() {
    var subscribersCapturing = {};
    var subscribersBubbling = {};
    var root = {};

    var callCapturing = function(path) {
        var paths = path.split(".");
        var completePath = null;
        for (var item in paths) {
            if (completePath == null) {
                completePath = paths[item]
            } else {
                completePath = completePath + "." + paths[item];
            }
            var subscribers = subscribersCapturing[completePath] || [];
            for (var s in subscribers) {
                subscribers[s]();
            }
        }
    };

    var callBubbling = function(path) {
        var paths = path.split(".");
        while (paths.length > 0) {
            var completePath = paths.join(".");
            var subscribers = subscribersBubbling[completePath] || [];
            for (var s in subscribers) {
                subscribers[s]();
            }
            paths = paths.slice(paths.length - 2, paths.length - 1);
        }
    };

    this.Set = function(path, value) {
        var paths = path.split(".");
        var node = root;
        for (var item in paths) {
            var nextNode = node[paths[item]];
            if (nextNode === undefined) {
                node[paths[item]] = {};
                nextNode = node[paths[item]];
            }
        }
        root[path] = value;
        callCapturing(path);
        callBubbling(path);
    };
    this.Get = function(path) {
        return root[path];
    };
    this.Capturing = function(path, callback) {
        var list = subscribersCapturing[path];
        if (list === undefined) {
            subscribersCapturing[path] = [callback];
        } else {
            list.push(callback);
        }
    };
    this.Bubbling = function(path, callback) {
        var list = subscribersBubbling[path];
        if (list === undefined) {
            subscribersBubbling[path] = [callback];
        } else {
            list.push(callback);
        }
    };
}

var h = function(tag, contents) {
    return "<" + tag + ">" + contents.toString() + "</" + tag + ">";
}

var context = new Context();
context.Set("CurrentUser.Name", "Daniel");

function render(user) {
    return h('div', user);
}

context.Capturing("CurrentUser", function() {
    var tree = render(context.Get("CurrentUser.Name"));
    document.getElementById("main").innerHTML = tree;
});

context.Set("CurrentUser.Name", "Daniel3");