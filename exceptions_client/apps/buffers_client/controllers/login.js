// ==========================================================================
// Project:   BuffersClient.login
// Copyright: ©2010 My Company, Inc.
// ==========================================================================
/*globals BuffersClient */

/** @class

  (Document Your Controller Here)

  @extends SC.Object
*/
BuffersClient.loginController = SC.ObjectController.create(
/** @scope BuffersClient.login.prototype */ {

    doLoad: function() {
        var bufferName = this.get('content').get('bufferName');

        var res = SC.Request.getUrl("/buffer/"+bufferName).json().notify(this,this.handleLoad).send() ;
    },

    handleLoad: function(res) {
        if(SC.ok(res)) {
            console.log("OK");

            // we show the loading page
            BuffersClient.getPath('loginPage.mainPane').remove();

            // let's show the interface
            BuffersClient.getPath('mainPage.mainPane').append();

            // we add the retrieved exceptions
            var exceptions = res.body();
            for(var i=0; i<exceptions.length; i++) {
                console.log("Inserting " + i);
                var exception = exceptions[i];
                BuffersClient.exceptionsController.addException(exception);
            }

        } else {
            alert("There was an error retrieving the buffer data, makes sure that the provided buffer name exists");
        }
    },

    doCreate: function() {
        var routes = BuffersClient.exceptionRoutesController.listOfRoutes();
        var emails = BuffersClient.emailsController.listOfEmails();
        var bufferName = this.get('content').get('bufferCreateName');
        var capacity = parseInt(this.get('content').get('capacity'));

        var requestData = { name: bufferName,
                            capacity: capacity,
                            exceptions: routes,
                            mails: emails };

        SC.Request.postUrl("/buffers").json().notify(this,this.handleCreate).send(requestData) ;
    },

    handleCreate: function(res) {
        if(SC.ok(res)) {
            alert("The buffer has been created successfully");
        } else {
            alert("There was an error creating the buffer");
        }
    }

}) ;
