import Inferno from 'inferno';
import h from 'inferno-hyperscript';
import R from "ramda";
import Freezer from "freezer-js";

//Hyperscript
const hdiv = (c) => h("div",{}, [c]);
const himg = (src, width) => h("img",{src:src,width:width});

// State Function
var state = new Freezer({});
const when = (path, callback) => {
    var node = R.path(path.split("."), state.get());
    node.getListener().on("update", (newState) => callback(newState.state));
    callback(node.state);
};
const set = (path,value) => {
    var p = path.split(".");
    R.path(p, state.get()).set("state", value);
};
const firstSet = (path, obj) => state.get().set(path,{state:obj});
const fetchSet = (url, path) => {
    fetch(url)
    .then(response => response.text())
    .then(response => {
        set(path, JSON.parse(response));
    });
};

//State from Server
const socket = new WebSocket('ws://localhost:5000');
socket.addEventListener('message', function (event) {
    var obj = JSON.parse(event.data);
    set(obj.Path, obj.Value);
});

//UI functions
const renderer = R.curry((node,component) => Inferno.render(component, node));
const render = R.curry((id, f) => R.pipe(f, renderer(document.getElementById(id))));

//APP functions
const initCurrentTime = (id) => firstSet(id, new Date());
const currentTime = (currentTime) => hdiv(currentTime.toString());

const pictureDescriptionWidgetInit = (id) => firstSet(id, {
    loading: true,
    pictureUrl: null,
    description: null
});
const pictureDescriptionWidget = (state) => {
    if(state.loading){
        return hdiv("loading...");
    }
    else{
        return hdiv([
            himg(state.pictureUrl, 300),
            hdiv("Description"),
            h("textarea")
        ]); 
    }
};

//Start
initCurrentTime("Client");
when("Client", render("client", currentTime));
setInterval(() => set("Client", new Date()), 1000);

initCurrentTime("Server");
when("Server", render("server", currentTime));

pictureDescriptionWidgetInit("currentPicture");
when("currentPicture", render("currentPicture", pictureDescriptionWidget));
fetchSet('/api/nextPicture', "currentPicture");