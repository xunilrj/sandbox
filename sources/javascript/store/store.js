function span(content)
{
  return `<span>${content}</span>`
}
class Store {
  constructor() {
    this.Root = {};    
    //this.Top = new Proxy();
  }
  
  get(uri, data) {
    data = data || {};
    const parent = this.Root;
    
    
    var handler = {
      get: (obj, prop) => {
        return obj[prop];
      },
      set: (obj, prop, value) => {        
        obj[prop] = value;
        let id = `${uri}.${prop}`;
        let el = document.getElementById(id);
        if(el){
          el.innerText = value;        
        }
        
        el = document.getElementById(uri);
        if(el){
          Object.keys(obj).forEach(x => {            
            let sel = el.querySelectorAll(`#${x}`);            
            sel.forEach(y => {              
              y.innerText = obj[x];
            });
          });
        }        
        return true;
      }
    };
    
    let newNode = new Proxy(data, handler);
    
    document.addEventListener("change", x => {
      if(x.target.id.startsWith(uri)){
        let tokens = x.target.id.split(["."]);
        let prop = tokens[tokens.length-1];        
        newNode[prop] = x.target.value;
      }
    });

    
    return newNode;
  }
}

let store = new Store();
let d1 = store.get("/objects/obj1", {Name:"Daniel"});
d1.Name = "Daniel2";
d1.Age = "32";