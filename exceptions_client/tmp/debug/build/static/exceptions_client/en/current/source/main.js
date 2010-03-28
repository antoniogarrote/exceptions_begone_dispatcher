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
ExceptionsClient.openWebSocket = openWebSocket = function(domain, port, path, keys, callbackFn, openFn, errorFn, unsupportedFn) {
    if ("WebSocket" in window) {

        var ws = new WebSocket("ws://"+ domain +":" + port + path);

        ws.onopen = function() {
            ws.send(keys);
            console.log("open");
            openFn(ws);
        };

        ws.onclose = function(evt) { errorFn(evt); };

        ws.onmessage = function(evt) {
            var json = eval("("+evt.data+")");
            callbackFn(json);
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
    ExceptionsClient.getPath('loginPage.mainPane').append() ;


    // Step 2. Set the content property on your primary controller.
    // This will make your app come alive!

    // We setup the login controller to the exception route being edited
    var exceptionRoute = ExceptionsClient.ExceptionRoute.create({ exceptionController: '*', exceptionMethod: '*' });
    ExceptionsClient.loginController.set('content', exceptionRoute);

    var query = SC.Query.local(ExceptionsClient.Exception, { orderBy: 'timestamp DESC' });
    var exceptions = ExceptionsClient.store.find(query);
    ExceptionsClient.exceptionsController.set('content', exceptions);

    var exceptionRoutes = ExceptionsClient.store.find(ExceptionsClient.ExceptionRoute);
    ExceptionsClient.exceptionRoutesController.set('content',exceptionRoutes);
} ;

function main() { ExceptionsClient.main(); }


; if ((typeof SC !== 'undefined') && SC && SC.scriptDidLoad) SC.scriptDidLoad('exceptions_client');