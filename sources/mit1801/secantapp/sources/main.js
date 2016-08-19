var React = require('react');
var ReactDOM = require('react-dom');
var componentRegistry = {
    "App": require('./components/app')
};
var elements = document.getElementsByClassName("react-element");
for (var i = 0; i < elements.length; i++) {
    var htmlElement = elements[i];
    var reactElementName = htmlElement.dataset.reactElement;
    var reactElement = React.createElement(componentRegistry[reactElementName], null);
    ReactDOM.render(reactElement, htmlElement);    
}