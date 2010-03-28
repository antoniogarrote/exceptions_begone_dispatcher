// ==========================================================================
// Project:   ExceptionsClient.loginController
// Copyright: ©2010 My Company, Inc.
// ==========================================================================
/*globals ExceptionsClient */

sc_require('configuration');

/** @class

  (Document Your Controller Here)

  @extends SC.Object
*/
ExceptionsClient.loginController = SC.ObjectController.create(
/** @scope ExceptionsClient.loginController.prototype */ {

    doLogin: function() {
        var routes = ExceptionsClient.exceptionRoutesController.listOfRoutes();
        if(routes.length > 0) {
            // we show the loading page
            ExceptionsClient.getPath('loginPage.mainPane').remove();
            ExceptionsClient.getPath('loadingPage.mainPane').append();


            var json = JSON.stringify(routes);

            // we can start the connection to the Dispatcher
            ExceptionsClient.openWebSocket(ExceptionsClient.WebSocketsConfiguration.domain,
                                           ExceptionsClient.WebSocketsConfiguration.port,
                                           ExceptionsClient.WebSocketsConfiguration.path,
                                           json,
                                           ExceptionsClient.exceptionsController.newException,
                                           this.connectionOpen,
                                           ExceptionsClient.exceptionsController.websocketsConnectionClose,
                                           this.unsupported);
        } else {
            alert("You must insert some combination of controller and action to proceed");
        }
    },

    connectionOpen: function(socket) {

        // we set the socket
        ExceptionsClient.exceptionsController.setConnection(socket);

        ExceptionsClient.getPath('loadingPage.mainPane').remove();
        ExceptionsClient.getPath('mainPage.mainPane').append();
    },

    connectionUnsupported: function() {
        ExceptionsClient.getPath('loadingPage.mainPane').remove();
        alert("Your browser does not support web sockets! Try this application with Chrome or the latest release of WebKit");
    }

}) ;
