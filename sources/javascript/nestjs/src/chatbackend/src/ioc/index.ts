import { Scope, Request } from "@nestjs/common";
import { REQUEST } from '@nestjs/core';

function fwd(target, name, key, f, interceptors, functions)
{
    return (...args) => {
        let i = 0;
        const next = () => {
            if(interceptors && i < interceptors.length)
            {
                let name = "";
                let config = null;
                if(interceptors[i].name)
                {
                    name = interceptors[i].name;
                    config = interceptors[i].config
                }
                else
                {
                    name = interceptors[i];
                }
                ++i;            
                return functions[name].apply(target, [config, target, name, key, next, ...args]);
            }
            else
            {
                return f.apply(target, args);
            }
        };
        return next();
    };
}

export function ioc(root, functions)
{
    let arr = [];
    const obj = {
        transient: (c, i = null) => {
            arr.push(c);
            if(i)
            {
                arr.push({
                    scope: Scope.TRANSIENT,
                    provide: i,
                    useFactory: (instance, req) => {     
                        let config = null
                        if(root && root.config)
                            config = Object.assign(config || {}, root.config);
                        if(req.headers["x-app-config"])
                            config = Object.assign(config || {}, JSON.parse(req.headers["x-app-config"]));
                        if(!config) return instance;

                        return new Proxy(instance, {
                            get: function (target, key) {
                                const p = target[key];
                                let interceptors = [];
                                interceptors = interceptors.concat(config[`*`] || []);
                                interceptors = interceptors.concat(config[`${i.name}.*`] || []);
                                interceptors = interceptors.concat(config[`${i.name}.${key.toString()}`] || []);
                                if (typeof p === "function")
                                    return fwd(target, i.name, key, p, interceptors, functions);
                                else return p;
                            }
                        });
                    },
                    inject: [c, REQUEST]
                });
            }
            return obj;
        },
        build: () => arr,
    }
    return obj;
}