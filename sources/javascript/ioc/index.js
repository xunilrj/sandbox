

function f({sender, ioc})
{
  console.log(sender, ioc);
}

function newSender({db})
{
  return {type:"sender", db, i:0};
}

function newDb()
{
  return {type:"db"};
}

function injectionProxy(ioc)
{
  var p = new Proxy({},{
    get: function(target, prop, receiver) {
      return ioc.resolve(prop);
    }
  });
  return p;
}

class IoC
{
  constructor(parent)
  {
    this.nexti = 0;
    this.parent = parent;
    this.reg = {};
    this.instances = {"ioc": this};

    if(!this.parent) this.name = "/root";
    else this.name = `${this.parent.name}/${this.parent.nexti}`;
  }

  register(name, f, {strategy} = {})
  {
    console.log(name, f.name, strategy);
    this.reg[name] = {builder:f, strategy};
  }

  instance(name, obj)
  {
    this.instances[name] = obj;
  }

  resolve(name, proxyioc)
  {
    proxyioc = proxyioc || this;

    console.log(name, this.name, "searching instance");
    const obj = this.instances[name];
    if(obj) return obj;

    console.log(name, this.name,"searching registry");
    const reg = this.reg[name];
    if(!reg && this.parent)
    {
      console.log(name, this.name, "going to parent");
      return this.parent.resolve(name, proxyioc);
    } 
    if(!reg) return null;

    console.log(name, this.name, "building using", proxyioc.name);
    const proxy = injectionProxy(proxyioc);
    obj = reg.builder(proxy);

    if(reg.strategy == "cache")
    {
      console.log(name, this.name, "caching at ", proxyioc.name);
      proxyioc.instances[name] = obj;
    }

    return obj;
  }

  proxy()
  {
    return injectionProxy(this);
  }

  createChild()
  {
    this.nexti++;
    return new IoC(this);
  }

  batch(obj)
  {
    Object.keys(obj).forEach(x => {
      const f = obj[x];
      this.register(
        x.replace("new","").toLowerCase(),
        f,
      );
    });
  }

  strategy(name, strategy)
  {
    this.reg[name].strategy = strategy;
  }
}

const root = new IoC();
// root.register("sender", newSender);
// root.register("db", newDb, {strategy:"cache"});
root.batch({newSender, newDb});
root.strategy("db", "cache");

const ctx = root.createChild();

f(ctx.proxy());
f(ctx.proxy());