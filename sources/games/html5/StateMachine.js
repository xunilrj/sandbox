export default function StateMachine(x)
{
  this.data = Object.create(x);
  this.stack = [];
  this.replace = function(newState, data) {

  };
  this.push = function(newState, newData) {
    let top = null
    if(this.stack.length > 0) {
      top = this.stack[this.stack.length - 1];    
    }

    const handler = this.states[newState];
    const initData = Object.assign(Object.create(this.data), newData);
    if(handler.init) handler.init(initData, this.stack);
    this.stack.push({
      name: newState,
      data: initData
    });

    if(top && this.states[top.name].exit) this.states[top.name].exit(top.data, this.stack);
    if(handler.enter) handler.enter(data, this.stack);
  };
  this.pop = function() {
    let top = null
    if(this.stack.length > 0) {
      top = this.stack[this.stack.length - 1];    
    }
    const handler = this.states[top];
    if(top && this.states[top.name].exit) this.states[top.name].exit(top.data, this.stack);
    

    let secondTop = null;
    let enter = null;
    if(this.stack.length > 1) {
      secondTop = this.stack[this.stack.length - 2];    
      enter = this.states[secondTop.name].enter
    }    
    if(secondTop && enter) enter(secondTop.data, this.stack);

    this.stack.pop();
  }

  this.states = {};
  this.addState = function(name, handler) {
    this.states[name] = handler;
  }

  this.current = new Proxy(this, {
    get: function(target, prop)
    {
      const top = target.stack[target.stack.length - 1];            
      return top.data[prop];
    },
    enumerate: function (target, sKey) {
      const top = target.stack[target.stack.length - 1];            
      return Object.keys(top.data);      
    },
    ownKeys: function (target, sKey) {
      const top = target.stack[target.stack.length - 1];            
      return Object.keys(top.data);      
    },
  });
}