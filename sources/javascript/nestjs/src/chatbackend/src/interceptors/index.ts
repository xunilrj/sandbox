export function log(cfg, target, name, key, next, ...args)
{
  if(cfg && (cfg["*"] || cfg[`${name}.*`] || cfg[`${name}.${key}`]))
    console.log(`${name}.${key.toString()}`, ...args);
  if(!cfg)
    console.log(`${name}.${key.toString()}`, ...args);
  return next();
}