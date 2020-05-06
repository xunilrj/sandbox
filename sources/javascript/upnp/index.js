const upnp = require('upnpjs');

upnp.getDevice().then(function(device) {
    console.log(device)
    /*
        Device is an object representation of your
        Internet Gateway Device in JavaScript.

        Follow Device Documentation bellow
    */
});