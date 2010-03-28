// ==========================================================================
// Project:   ExceptionsClient
// Copyright: ©2010 My Company, Inc.
// ==========================================================================
/*globals ExceptionsClient */

sc_require('json');

// This function starts a WS connection to the Exceptions Begone Dispatcher:
// - domain
// - port
// - path
// - keys: array of arrays with controller and action, [ ['AddressBooks', 'new'], ['Postings', '*']]
//         '*' can be used as a placeholder for all.
// - callbackFn: function that will be called when new new data can be read from the socket: callbackFn(event, exceptionData)
// - errorFn: error callback: errorFn(event)
// - unsupportedFn: function that will be called if the browser does not support web sockets: unsupportedFn()
ExceptionsClient.openWebSocket = openWebSocket = function(domain, port, path, keys, callbackFn, errorFn, unsupportedFn) {
    if ("WebSocket" in window) {

        var ws = new WebSocket("ws://"+ domain +":" + port + path);

        ws.onopen = function() {
            ws.send(keys);
            console.log("open");
        };

        ws.onclose = function(evt) { errorFn(evt); };

        ws.onmessage = function(evt) {
            //var json = eval("("+evt.data+")");
            //callbackFn(data);
            alert("ALGO!!! mas? " + evt.data );
            //callback(data);
        };

    } else {
        unsupportedFn();
    }
}

// This is the function that will start your app running.  The default
// implementation will load any fixtures you have created then instantiate
// your controllers and awake the elements on your page.
//
// As you develop your application you will probably want to override this.
// See comments for some pointers on what to do next.
//
ExceptionsClient.main = function main() {

  // Step 1: Instantiate Your Views
  // The default code here will make the mainPane for your application visible
  // on screen.  If you app gets any level of complexity, you will probably
  // create multiple pages and panes.
  ExceptionsClient.getPath('mainPage.mainPane').append() ;

  // Step 2. Set the content property on your primary controller.
  // This will make your app come alive!

  // TODO: Set the content property on your primary controller
  // ex: ExceptionsClient.contactsController.set('content',ExceptionsClient.contacts);
    var exceptions = ExceptionsClient.store.find(ExceptionsClient.Exception);
    ExceptionsClient.exceptionsController.set('content', exceptions);
} ;

function main() { ExceptionsClient.main(); }


