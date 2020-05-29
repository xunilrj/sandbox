import React, { useReducer, useContext, useEffect } from 'react';
import Freezer from 'freezer-js';
import curry from 'curry';

const StoreCtx = React.createContext();
export const Store = StoreCtx.Provider;

export function newStore() {
    const store = new Freezer({
    });
    return store;
}

const collectNames = function (f) {
    var names = [];
    const proxy = new Proxy({}, {
        get: function (target, k, receiver) {
            names.push(k);
            return receiver;
        }
    });
    f(proxy);
    return names;
}

const splitPath = function (s) {
    s = s.replace(/\[(\w+)\]/g, '.$1'); // convert indexes to properties
    s = s.replace(/^\./, '');           // strip a leading dot
    return s.split('.');
}

const byPath = function (o, path, f) {
    let lastParent;
    for (var i = 0, n = path.length; i < n; ++i) {
        if (!o) break;
        var k = path[i];

        lastParent = o;
        if (k in o) {
            o = o[k];
        } else {
            if (f)
                o = f(o, k, i == n - 1);
            else break;
        }
    }
    return [o, lastParent, path[path.length - 1]];
}

function logBA(f) {
    return (...args) => {
        console.log("before");
        f(...args);
        console.log("after");
    }
}

function returns(value, lastParent, lastKey) {
    if (typeof value === 'string' || value instanceof String) {
        let reset = (str) => () => lastParent.set(lastKey, str);
        return [value, reset];
    } else if (Array.isArray(value)) {
        let push = (...args) => () => value.push(...args);
        let splice = curry((a, b, items) => () => value.splice(a, b, ...items));
        let sort = (f) => () => value.sort(f);
        return [value, push, splice, sort];
    } else if (value instanceof Object) {
        let reset = (...args) => () => {
            value.reset(...args);
        };
        return [value, reset];
    }

    throw new Error("Not Implemented");
}

export function useInitialValue(path, initialValue) {
    path = collectNames(path);
    const store = useContext(StoreCtx);
    const state = store.get();
    byPath(state, path, (o, k, isLast) => {
        let v = o.set(k, isLast ? initialValue : {});
        return v[k];
    });
}

export function useNode(path, node) {
    path = collectNames(path);
    const store = useContext(StoreCtx);

    const [[value, lastParent, lastKey], dispatch] = useReducer(
        (old, next) => {
            return [next, lastParent, lastKey];
        }, path, x => {
            const state = node || store.get();
            return byPath(state, path);
        });

    useEffect(() => {
        let listener, onUpdate;
        if (value.getListener) {
            listener = value.getListener();
            onUpdate = (s, _) => {
                dispatch(s);
            }
        } else if (lastParent.getListener) {
            listener = lastParent.getListener();
            onUpdate = (s, _) => {
                dispatch(s[lastKey]);
            }
        }
        listener.on('update', onUpdate);
        return () => listener.off('update', onUpdate);
    }, [store]);

    return returns(value, lastParent, lastKey);
}

export function useState(path, node) {
    path = collectNames(path);
    let [value, lastParent, lastKey] = byPath(node, path);
    return returns(value, lastParent, lastKey);
}

export function useInput(path, node) {
    const names = collectNames(path);
    const [value, set] = useState(path, node);
    return {
        name: names[names.length - 1],
        defaultValue: value,
        onChange: e => {
            set(e.target.value)();
        }
    }
}

function Connect(f, Component, { }) {
    const [props, _] = useNode(f);
    return <Component {...props} />
}

export function mapToProps(f, Component) {
    return curry(Connect)(f, Component);
}   