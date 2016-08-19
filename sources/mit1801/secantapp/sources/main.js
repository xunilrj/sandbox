var ready = require('./libs/ready');
var React = require('react');
var ReactDOM = require('react-dom');
var componentRegistry = {
    "App": require('./components/app')
};
ready.run(".react-element", function(htmlElement){
    console.log('Identified new React Element');
    var reactElementName = htmlElement.dataset.reactElement;
    if(reactElementName == undefined){
        console.warn("React Element mal formed. Fill data-react-element attribute");
    }else{
        var reactElementName = htmlElement.dataset.reactElement;
        var reactElement = React.createElement(componentRegistry[reactElementName], null);
        ReactDOM.render(reactElement, htmlElement);
    }    
});