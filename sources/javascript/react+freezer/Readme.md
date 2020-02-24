# Building a React + Freezer integration

We are aiming here a Flux architecture using Freezer as the store. Why? Because of its simplicity, and its guarantee of the immutability of untouched nodes. See more at https://github.com/arqex/freezer#why-another-state-holder.  

## Quick Freezer tutorial

The "hello world" for Freezer is:

```js
let store = new Freezer({user:{name:"Daniel"}, someOtherData:{}});
let state = store.get();

console.log(state.user.name); // "Daniel"

state.user.name = "Daniel 2"; //Error: Cannot assign to read only property 'name' of object '#<Object>'
```

So, if you cannot assign new values, how do you change the store?

```js
let newUser = state.user.reset({name: "Daniel 2"});
console.log(newUser.name); // "Daniel 2"
```

Maybe you love it, maybe you hate it, but what is the advantage? 

Look below, we never touch "someOtherData" so the node does not change. That gives us the knowledge of exactly which nodes changed; this allows us to update the UI for these parts, and for these parts only.  

```js
let before =  store.get().user;
let newUser = state.user.reset({name: "Daniel 2"});
let after =  store.get().user;
console.log(before == after); //false

before =  store.get().someOtherData;
let newUser = state.user.reset({name: "Daniel 3"});
after =  store.get().someOtherData;
console.log(before == after); //true
```

Freezer also has a nice "bus" that we can use like this:

```js
store.on("changeUser", (newUser) => {
  store.get().user.reset(newUser);
});
store.emit("changeUser",{name:"Daniel 4"});
console.log(store.get().user.name); //Daniel 4
```

and one of the events of this bus is when something change:

```js
store.on("update", (current,old) => console.log("update",current,old));
store.get().user.getListener()
  .on("update", (current,old)  => console.log("user:update",current,old));
store.get().someOtherData.getListener()
  .on("update", (current,old) => console.log("someOtherData:update",current,old));
store.emit("changeUser",{name:"Daniel 5"});
```

This will generate

```
update {user: {…}, someOtherData: {…}} {user: {…}, someOtherData: {…}}
user:update {name: "Daniel 5"} {name: "Daniel 4"}
```

We never receive the update for the "someOtherData", which makes sense because we never updated it. This will also prove useful when integrating with React.

That is pretty much everything we need to know about Freezer.


## Integration version 01 - React Context

The simplest possible integration with React is using React Context (https://reactjs.org/docs/context.html). 

Context work in pairs: you have one component that provides the data, and one that consumes and update itself on data changes.

First you have to create the context, get the Provider and render it;

```js
class FreezerProvider extends React.Component
{
  render()
  {
    const state = store.get();
    const {Provider} = storeCtx;
    let children = this.props.children;
    if(children) React.cloneElement(children);
    return (<Provider value={state}>{children}</Provider>);
  }
}
```

and secondly, we need the Consumer;

```js
class FreezerConsumer extends React.Component
{
  renderChildren(state)
  {
    let children = this.props.children;
    if(children) children = React.cloneElement(children, state);
    return children;
  }
  render()
  {
    const {Consumer} = storeCtx;
    return (<Consumer>{this.renderChildren.bind(this)}</Consumer>);
  }
}
```

Now we wire them together like this:

```js

render((
  <FreezerProvider> 
    <div>
      <FreezerConsumer>
      </FreezerConsumer>
    </div>
  </FreezerProvider>
), document.getElementById('app'));
```

More realistically, we would have a component inside the Consumer.

```js
class UserStatus extends React.Component
{
  render()
  {
    console.log(this.props);
    return <div>{this.props.user.name}</div>
  }
}

render((
  <FreezerProvider> 
    <FreezerConsumer>
      <UserStatus/>
    </FreezerConsumer>
  </FreezerProvider>
), document.getElementById('app'));
```

The first obvious problem is that the whole "store" state is being passed to "UserStatus". The component itself need to know its schema. Not good.

We can improve this by making "FreezerConsumer" receive a map. It will map the "store" state to what "UserStatus" needs.


```js
class FreezerConsumer extends React.Component
{
  renderChildren(state)
  {
    const map = this.props.map;
    if(!map) map = ((x) => x);

    state = map(state);

    let children = this.props.children;
    if(children) children = React.cloneElement(children, state);

    return children;
  }
  render()
  {
    const {Consumer} = storeCtx;
    return (<Consumer>{this.renderChildren.bind(this)}</Consumer>);
  }
}

class UserStatus extends React.Component
{
  render()
  {
    return <div>{this.props.name}</div>
  }
}
```

Now "UserStatus" is completely ignorant from where its data comes from. As it should.

## Integration version 02 - Updates

First, let us see the problem. We expect that after changing the store, our UI would update itself. This does not happen, and the reason is pretty obvious. React has no idea that the "store" was updated.

```js
render((
  <FreezerProvider> 
    <FreezerConsumer>
      <UserStatus/>
    </FreezerConsumer>
  </FreezerProvider>
), document.getElementById('app'));
let newUser = store.get().user.reset({name: "Daniel 2"});
```

React Context guarantees that when the "Provider" "value" change the "Consumer" will be rendered again.

Remember that we have a very simple way of being notified when the "store" change. So let us wire these two features together, and we need just one line.

```js
class FreezerProvider extends React.Component
{
  componentDidMount()
  {
    store.on("update", () => this.forceUpdate());
  }
  render()
  {
    const state = store.get();
    const {Provider} = storeCtx;
    let children = this.props.children;
    if(children) React.cloneElement(children);
    return (<Provider value={state}>{children}</Provider>);
  }
}
```

Now our UI update itself automatically.

## Integration version 03 - Commands

Now we need more interaction. Suppose that the control starts with a "login" button, we somehow log the user and then show its name.

```js
class UserStatus extends React.Component
{
  render()
  {
    const {isAuthenticated, name} = this.props;
    if(!isAuthenticated)
      return <button>Login</button>
    else
      return <div>{name}</div>
  }
}
```
One simple and easy way is just to "emit" a Freezer event from the button. We are keeping as simple as possible here. Realistically you would get this value from inputs, off course.

```js
store.on("login", ({name,password}) => {  
  store.get().user.reset({isAuthenticated: true, name});
});

class UserStatus extends React.Component
{
  render()
  {
    const {isAuthenticated, name} = this.props;
    const loginEvent = {name:"daniel",password:"12345"};
    if(!isAuthenticated)
      return <button onClick={x=> store.emit("login",loginEvent)}>Login</button>
    else
      return <div>{name}</div>
  }
}
```

And that is it. But what if we needed to call the server and deal with Promises?

```js
function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}
store.on("login", async ({name,password}) => {  
  store.get().user.reset({isLoading: true});
  await sleep(2000);
  store.get().user.reset({isAuthenticated: true, name});
});

class UserStatus extends React.Component
{
  render()
  {
    const {isLoading, isAuthenticated, name} = this.props;
    const loginEvent = {name:"daniel",password:"12345"};

    if(isLoading) return <div>...</div>;
    if(!isAuthenticated)
      return <button onClick={x=> store.emit("login",loginEvent)}>Login</button>
    else
      return <div>{name}</div>
  }
}
```

Done!

## Integration version 03 - Minimizing Re-Renderings

First, we need to verify what would happen if we had a second Component. This Component does nothing, just log when it is rendering. We never change "someOtherData" from the "store", so we are expecting only one log entry.

```js
class SomeOtherData extends React.Component
{
  render()
  {
    console.log("SomeOtherData.render", this.props.someOtherData);
    return <div>someOtherData</div>;
  }
}


render((
  <FreezerProvider> 
    <FreezerConsumer map={x => x.user}>
      <UserStatus/>
    </FreezerConsumer>
    <FreezerConsumer map={x => ({someOtherData:x.someOtherData})}>
      <SomeOtherData/>
    </FreezerConsumer>
  </FreezerProvider>
), document.getElementById('app'));
```

Result:
```
SomeOtherData.render {}
SomeOtherData.render {}
SomeOtherData.render {}
```

Fail! We render this Component three times. Why?

Well... if you remember our setup, we subscribed to every "store" update, and we always pass the new "store" state to React. If we want to stop rendering needlessly, there are a couple of strategies we can try.

First: use the fact that Freezer minimized mutation on the store and use "React" method "shouldComponentUpdate". (https://reactjs.org/docs/react-component.html#shouldcomponentupdate)

We will only update when the "someOtherData" changes, in our case here, never.

```js
class SomeOtherData extends React.Component
{
  shouldComponentUpdate(nextProps, nextState)
  {
    return this.props.someOtherData != nextProps.someOtherData;
  }
  render()
  {
    console.log("SomeOtherData.render", this.props.someOtherData);
    return <div>someOtherData</div>;
  }
}
```

Result:
```
SomeOtherData.render {}
```

Just one render. Beautiful!

Off course is not practical to do this in every Component. And perceive that I had to receive an object, a node from the "store" to compare. I want to keep my Components ignorant of "stores", even subtleties like this one.

The second approach does the exact same thing but in another place. We can start with the Consumer.

The problem is that inside the Consumer is already too late. The Provider already asked us to re-render. We are analyzing the state inside the "renderChildren", we need to return anything, we cannot abort.

So let us try inside the Provider. Is the "forceUpdate()" inside the Provider that started everything. And the problem is that we are stuck in a "One Provider" for multiple "Consumers" model.

The question is: do we need "React Context"? The answer is: No!

We can very quickly solve this problem with just one simple Component.

```js
class FreezerConnect extends React.Component
{
  getMapped()
  {
    let state = store.get();

    const map = this.props.map;
    if(!map) map = ((x) => x);
    return map(state);
  }

  componentDidMount()
  {
    const state = this.getMapped();
    if(!Array.isArray(state)) state = [state];
    state.forEach(x => {
      if(x.getListener)
        x.getListener().on("update", () => this.forceUpdate());
    });
  }

  render()
  {
    const state = this.getMapped();    
    let children = this.props.children;
    if(children) children = React.cloneElement(children, state);
    return children;
  }
}

render((
  <>
    <FreezerConnect map={x => x.user}>
      <UserStatus/>
    </FreezerConnect>
    <FreezerConnect map={x => x.someOtherData}>
      <SomeOtherData/>
    </FreezerConnect>
  </>
), document.getElementById('app'));
```

I am calling "Connect" because it is the same name of the "Redux" (https://react-redux.js.org/using-react-redux/connect-mapstate) that is pretty much what we are doing.

But, what if I want to return something that is not a "store node". For example, I want to join data.

That is why We made "FreezerConnect" accepting an array on map. We can make this in two steps here: "map" and "reduce". First, we run "map", and we subscribe to all listeners (just once) and after this, we "reduce" this array and pass it to its children.

```js
class FreezerConnect extends React.Component
{
  getMapped()
  {
    let state = store.get();

    const map = this.props.map;
    if(!map) map = ((x) => x);

    return map(state);
  }

  getReduced()
  {
    const state = this.getMapped();
    const reduce = this.props.reduce;
    if(!reduce) reduce = ((x) => x);
    return reduce(state);
  }

  componentDidMount()
  {
    const state = this.getMapped();
    if(!Array.isArray(state)) state = [state];
    state.forEach(x => {
      if(x.getListener)
        x.getListener().on("update", () => this.forceUpdate());
    });
  }

  render()
  {
    const state = this.getReduced();    
    let children = this.props.children;
    if(children) children = React.cloneElement(children, state);
    return children;
  }
}

class JoinUserAndSomeOhterData extends React.Component
{
  render()
  {
    console.log("JoinUserAndSomeOhterData.render", this.props);
    return <div></div>;
  }
}
```

Now, this new Component renders three times as expected:
1 - First time;
2 - When we change the user to isLoading=true;
3 - When we change the user to name = "Daniel";

And the "SomeOtherData" is rendered just once because we never change it. Not bad.

Result:
```
JoinUserAndSomeOhterData.render {user: {…}, someOtherData: {…}}
JoinUserAndSomeOhterData.render {user: {…}, someOtherData: {…}}
JoinUserAndSomeOhterData.render {user: {…}, someOtherData: {…}}
```

## Integration version 04 - Store Location

We still have one problem. We are still getting the store using a closure. The most natural approach is, off course, to pass it using props.

```js
class FreezerConnect extends React.Component
{
  getMapped()
  {
    let state = this.props.store.get();
    ...
    return state;
  }

  ...
}

render((
  <>
    <FreezerConnect store={store} map={x => x.user}>
      <UserStatus/>
    </FreezerConnect>
    <FreezerConnect store={store} map={x => x.someOtherData}>
      <SomeOtherData/>
    </FreezerConnect>
     <FreezerConnect store={store} 
      map={x => [x.user,x.someOtherData]}
      reduce={([user,someOtherData]) => ({user,someOtherData})}>
      <JoinUserAndSomeOhterData/>
    </FreezerConnect>
  </>
), document.getElementById('app'));
```

## Integration version 05 - Better Emit

To be honest, our emit is easy to use, but I still think we can do better.

Let us remember how we are doing it.

```js
class UserStatus extends React.Component
{
  render()
  {
    const {isLoading, isAuthenticated, name} = this.props;
    const loginEvent = {name:"daniel",password:"12345"};

    if(isLoading) return <div>...</div>;
    if(!isAuthenticated)
      return <button onClick={x=> store.emit("login",loginEvent)}>Login</button>
    else
      return <div>{name}</div>
  }
}
```

What we are doing here is creating a new function every time we render this Component (https://reactjs.org/docs/faq-functions.html#arrow-function-in-render). Not ideal, but it is the easiest way to bind event handlers.

But this code has two bad traits:
1 - It points directly to the store;
2 - It does not expose the "login event" by props.

Again, we want our components to be as ignorant as possible from "stores". So let us try to improve this.

First, we will just extract this to a new method:

```js
function emit(type, e)
{
  return x => store.emit(type,e);
}

class UserStatus extends React.Component
{
  render()
  {
    const {isLoading, isAuthenticated, name} = this.props;
    const loginEvent = {name:"daniel",password:"12345"};

    if(isLoading) return <div>...</div>;
    if(!isAuthenticated)
      return <button onClick={emit("login", loginEvent)}>Login</button>
    else
      return <div>{name}</div>
  }
}
```

Our use is already simpler. If we make the "emit" function magically find the "store" our first bullet is done.

Let us swap to the second one.

We want our login event to be "props-acessible". One easy way is to make the "emit" search and call the callback on "this.props". All we need to do is:

```js
function emit(props, type, e)
{
  let f = props[type];
  return x =>
  {
    if(f) f(e);
    store.emit(type,e);
  } 
}

class UserStatus extends React.Component
{
  render()
  {
    console.log("UserStatus.render", this.props);
    const {isLoading, isAuthenticated, name} = this.props;
    const loginEvent = {name:"daniel",password:"12345"};

    if(isLoading) return <div>...</div>;
    if(!isAuthenticated)
      return <button onClick={emit(this.props, "login", loginEvent)}>Login</button>
    else
      return <div>{name}</div>
  }
}

render((
  <>
    <FreezerConnect store={store} map={x => x.user}>
      <UserStatus login={console.log}/>
    </FreezerConnect>
    <FreezerConnect store={store} map={x => x.someOtherData}>
      <SomeOtherData/>
    </FreezerConnect>
     <FreezerConnect store={store} 
      map={x => [x.user,x.someOtherData]}
      reduce={([user,someOtherData]) => ({user,someOtherData})}>
      <JoinUserAndSomeOhterData/>
    </FreezerConnect>
  </>
), document.getElementById('app'));
```

And that is it.

Exposing the callback like this enable us to do this:

```js
render((
  <>
    <FreezerConnect store={store} map={x => x.user}>
      <UserStatus login={store.emit.bind(store, "login")}/>
    </FreezerConnect>
    <FreezerConnect store={store} map={x => x.someOtherData}>
      <SomeOtherData/>
    </FreezerConnect>
     <FreezerConnect store={store} 
      map={x => [x.user,x.someOtherData]}
      reduce={([user,someOtherData]) => ({user,someOtherData})}>
      <JoinUserAndSomeOhterData/>
    </FreezerConnect>
  </>
), document.getElementById('app'));
```

What we gain? The store does not need to be known inside emit, nor inside the Component.

```js
function emit(props, type, e)
{
  let f = props[type];
  return x => { if(f) f(e); } 
}
```

But we can simplify this making our emit a little bit smarter. For example:

```js
function emit(props, type, e)
{
  let f = props[type];
  return x =>
  {
    if(f.emit) f.emit(type, e);
    else if(f) f(e);
  } 
}

render((
  <>
    <FreezerConnect store={store} map={x => x.user}>
      <UserStatus login={store}/>
    </FreezerConnect>
    <FreezerConnect store={store} map={x => x.someOtherData}>
      <SomeOtherData/>
    </FreezerConnect>
     <FreezerConnect store={store} 
      map={x => [x.user,x.someOtherData]}
      reduce={([user,someOtherData]) => ({user,someOtherData})}>
      <JoinUserAndSomeOhterData/>
    </FreezerConnect>
  </>
), document.getElementById('app'));
```

Now, if we pass a function, we call it. If we pass a store, we "emit" the event.

## Integration version 06 - Store of Stores

This is a polemic one. It is not hard to find guides saying never to use multiple stores. It is even easier to find guides saying never to touch the "window".

Here we are going to break both rules. This will allow an easier convention that will make the code easier. Use it judiciously.

We have "store={store}" in a lot of places. And the vast majority of applications only have one store. So why not use the good-or-bad singleton pattern here. In my experience, singletons tend to cause fewer problems in single-user applications.

We will store our store inside "window.stores".

```js
window.stores = {
  default: store
};
```

And in every place that we need a store, we use the one provided, or we search there the named passed. This allows us the remove a lot of "store={store}". 

```js
class FreezerConnect extends React.Component
{
  getMapped()
  {
    let store = this.props.store;
    if(!store) store = "default";
    if (typeof store === 'string' || store instanceof String)
      store = window.stores[store];

    let state = store.get();

    const map = this.props.map;
    if(!map) map = ((x) => x);

    state = map(state);
    return state;
  }

  ...
}

render((
  <>
    <FreezerConnect map={x => x.user}>
      <UserStatus login={store}/>
    </FreezerConnect>
    <FreezerConnect map={x => x.someOtherData}>
      <SomeOtherData/>
    </FreezerConnect>
     <FreezerConnect
      map={x => [x.user,x.someOtherData]}
      reduce={([user,someOtherData]) => ({user,someOtherData})}>
      <JoinUserAndSomeOhterData/>
    </FreezerConnect>
  </>
), document.getElementById('app'));
```

But we still need to pass the store to the callbacks. Can we remove it?

## Integration version 07 - Event Bubbling

This one is also a polemic one. People in the React world tend to avoid DOM events. They prefer to stick with Components callbacks.

But here we will use the DOM Event bubbling and use the DOM root as a global listener. See more at "3.1. Event dispatch and DOM event flow" at https://www.w3.org/TR/DOM-Level-3-Events.

To achieve this, we will change a little bit our emit. Now we basically
1 - search for an emit method. Call it, if found;
2 - otherwise call it, if it is a function;
3 - we dispatch the event with "bubbles" enabled.

```js
function emit(props, type, detail, options)
{
  const {emit = true, dispatch = true} = options || {};
  const f = props[type];
  const event = new CustomEvent(type, { bubbles: true, detail });
  return e =>
  {
    if(emit && f && f.emit) f.emit(type, detail);
    else if(typeof f === "function") f(detail);
    if(dispatch && e && e.target && e.target.dispatchEvent)
      e.target.dispatchEvent(event);
  } 
}
```

We still need to "hear" this event somehow. We will use our FreezerConnect.


```js

class FreezerConnect extends React.Component
{
  getStore()
  {
    let store = this.props.store;
    if(!store) store = "default";
    if (typeof store === 'string' || store instanceof String)
      store = window.stores[store];
    return store;
  }
  
  ...

  componentDidMount()
  {
    ...

    if(this.props.events)
    {
      const events = this.props.events.split(",");
      events.forEach(x => {
        window.document.addEventListener(x, e => {
          const store = this.getStore();
          if(store && store.emit)
            store.emit(e.type, e.detail);
        });
      });
    }
  }

  ...
}
```

This allows us to "hear" all events. We can choose a better node, instead of the DOM root, but our "FreezerConnect" does not generate any DOM node. Something fixable in the future. For now, it works.

This allows us to do:

```js
render((
  <>
    <FreezerConnect map={x => x.user} events="login">
      <UserStatus/>
    </FreezerConnect>
    <FreezerConnect map={x => x.someOtherData}>
      <SomeOtherData/>
    </FreezerConnect>
     <FreezerConnect
      map={x => [x.user,x.someOtherData]}
      reduce={([user,someOtherData]) => ({user,someOtherData})}>
      <JoinUserAndSomeOhterData/>
    </FreezerConnect>
  </>
), document.getElementById('app'));
```

## Final Code

```js
function emit(props, type, detail, options)
{
  const {emit = true, dispatch = true} = options || {};
  const f = props[type];
  const event = new CustomEvent(type, { bubbles: true, detail });
  return e =>
  {
    if(emit && f && f.emit) f.emit(type, detail);
    else if(typeof f === "function") f(detail);
    if(dispatch && e && e.target && e.target.dispatchEvent)
      e.target.dispatchEvent(event);
  } 
}

class FreezerConnect extends React.Component
{
  getStore()
  {
    let store = this.props.store;
    if ((!store) || (typeof store === 'string' || store instanceof String))
        if(window && window.stores)
          store = window.stores[store || "default"];
    return store;
  }

  getMapped()
  {
    const store = this.getStore();
    const state = store.get();
    const map = this.props.map || ((x) => x);
    return map(state);
  }

  getReduced()
  {
    const state = this.getMapped();
    const reduce = this.props.reduce || ((x) => x);
    return reduce(state);
  }

  componentDidMount()
  {
    const state = this.getMapped();
    if(!Array.isArray(state)) state = [state];
    state.forEach(x => {
      if(x.getListener)
        x.getListener().on("update", () => this.forceUpdate());
    });

    if(this.props.events)
    {
      const events = this.props.events.split(",");
      events.forEach(x => {
        window.document.addEventListener(x, e => {
          const store = this.getStore();
          if(store && store.emit)
            store.emit(e.type, e.detail);
        });
      });
    }
  }

  render()
  {
    const state = this.getReduced();    
    let children = this.props.children;
    if(children) children = React.cloneElement(children, state);
    return children;
  }
}
```

# Adapting it to Preact

Preact is a lightweight version of React and has everything we need. The advantage can be better seen when you compare React+ReactDOM to Preact.

https://bundlephobia.com/scan-results?packages=freezer-js@0.14.1,react@16.12.0,react-dom@16.12.0


```
freezer-js 3.6 kB MIN + GZIP
react 2.6 kB MIN + GZIP
react-dom  36.2 kB MIN + GZIP
TOTAL 42.4 kB MIN + GZIP
```

versus

```
freezer-js 3.6 kB MIN + GZIP
preact 3.7 kB MIN + GZIP
TOTAL 7.3 kB MIN + GZIP
```

The adaptation is very simple. Just remove "React" in front of everything.


```js
/** @jsx h */

import Freezer from 'freezer-js';
import {render, h, Component, cloneElement} from 'preact';

let store = new Freezer({user:{name:"Daniel"}, someOtherData:{}});
window.stores = { default: store };

function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}
store.on("login", async ({name,password}) => {  
  store.get().user.reset({isLoading: true});
  await sleep(2000);
  store.get().user.reset({isAuthenticated: true, name});
});

function emit(props, type, detail, options)
{
  const {emit = true, dispatch = true} = options || {};
  const f = props[type];
  const event = new CustomEvent(type, { bubbles: true, detail });
  return e =>
  {
    if(emit && f && f.emit) f.emit(type, detail);
    else if(typeof f === "function") f(detail);
    if(dispatch && e && e.target && e.target.dispatchEvent)
      e.target.dispatchEvent(event);
  } 
}

class FreezerConnect extends Component
{
  getStore()
  {
    let store = this.props.store;
    if ((!store) || (typeof store === 'string' || store instanceof String))
        if(window && window.stores)
          store = window.stores[store || "default"];
    return store;
  }

  getMapped()
  {
    const store = this.getStore();
    const state = store.get();
    const map = this.props.map || ((x) => x);
    return map(state);
  }

  getReduced()
  {
    const state = this.getMapped();
    const reduce = this.props.reduce || ((x) => x);
    return reduce(state);
  }

  componentDidMount()
  {
    const state = this.getMapped();
    if(!Array.isArray(state)) state = [state];
    state.forEach(x => {
      if(x.getListener)
        x.getListener().on("update", () => this.forceUpdate());
    });

    if(this.props.events)
    {
      const events = this.props.events.split(",");
      events.forEach(x => {
        window.document.addEventListener(x, e => {
          const store = this.getStore();
          if(store && store.emit)
            store.emit(e.type, e.detail);
        });
      });
    }
  }

  render()
  {
    const state = this.getReduced();    
    let children = this.props.children;
    if(children) children = cloneElement(children, state);
    return children;
  }
}

class UserStatus extends Component
{
  render()
  {
    const {isLoading, isAuthenticated, name} = this.props;
    const loginEvent = {name:"daniel",password:"12345"};

    if(isLoading) return <div>...</div>;
    if(!isAuthenticated)
      return <button onClick={x=> store.emit("login",loginEvent)}>Login</button>
    else
      return <div>{name}</div>
  }
}

class SomeOtherData extends Component
{
  render()
  {
    console.log("SomeOtherData.render", this.props.someOtherData);
    return <div>someOtherData</div>;
  }
}

render((
  <div>
    <FreezerConnect map={x => x.user} events="login">
      <UserStatus/>
    </FreezerConnect>
    <FreezerConnect map={x => x.someOtherData}>
      <SomeOtherData/>
    </FreezerConnect>
  </div>
), document.getElementById('app'));
```

# Improvements

1 - Unregister Freezer callbacks with "off";  
2 - New Component just for events;  
  - Create DOM Node when node=true;  
  - Stop event propagation when stopPropagation=true;  
  - Events could also be a array instead of comma separated string;
  - Each event can have its stopPropagation config;
  
3 - Improve map/reduce. "map" should return an object instead of an array. This will make the Identity reduce more useful;  
4 - Not totally related but standard methods to organize the store;
  - addDataTable: normalize json array to data table like schema;
  - addConfig: general Component config with tree inheretance;
  - addTask: admin promises like redux-thunk and redux-saga.

5 - Make this work with preact/InfernoJS and plain hyperscrit

6-  
