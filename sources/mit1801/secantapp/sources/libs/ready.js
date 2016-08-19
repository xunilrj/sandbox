'use strict';

var listeners = [],
MutationObserver = window.MutationObserver || window.WebKitMutationObserver,
observer;

var check = function(){
    // Check the DOM for elements matching a stored selector
    for(var i = 0, len = listeners.length, listener, elements; i < len; i++){
        listener = listeners[i];
        // Query for elements matching the specified selector
        elements = document.querySelectorAll(listener.selector);
        for(var j = 0, jLen = elements.length, element; j < jLen; j++){
            element = elements[j];
            // Make sure the callback isn't invoked with the 
            // same element more than once
            if(!element.ready){
                element.ready = true;
                // Invoke the callback with the element
                listener.fn.call(element, element);
            }
        }
    }
};

module.exports = {
    run: function (selector, fn){
        // Store the selector and callback to be monitored
        listeners.push({
            selector: selector,
            fn: fn
        });
        if(!observer){
            // Watch for changes in the document
            observer = new MutationObserver(check);
            observer.observe(document.documentElement, {
                childList: true,
                subtree: true
            });
        }
        // Check if the element is currently in the DOM
        check();
    }
}; 