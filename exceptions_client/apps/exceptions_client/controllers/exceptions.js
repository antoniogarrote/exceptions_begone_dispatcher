// ==========================================================================
// Project:   ExceptionsClient.exceptionsController
// Copyright: ©2010 My Company, Inc.
// ==========================================================================
/*globals ExceptionsClient */

/** @class

  (Document Your Controller Here)

  @extends SC.ArrayController
*/
ExceptionsClient.exceptionsController = SC.ArrayController.create(
/** @scope ExceptionsClient.exceptionsController.prototype */ {

    // variables

    connectionUri: ('connected to: ws://' + ExceptionsClient.WebSocketsConfiguration['domain'] + ':' + ExceptionsClient.WebSocketsConfiguration['port'] + '/' + ExceptionsClient.WebSocketsConfiguration['path']),

    totalExceptionCount: 0,
    selectedException: null,

    // CRUD operations

    addException: function(data) {
        SC.RunLoop.begin(); // running this events in a new run loop to get inmediate UI refresh

        var json = data.notification.payload;
        json['identifier'] = data.notification.identifier;

        var exceptions = this.get('content') || SC.Array.create();

        var exception = exceptions.find(function(item, index, enumerable) {
            return item.get('identifier') == json['identifier'];
        });

        var date = SC.DateTime.create();

        var oldCount = this.get('totalExceptionCount');
        this.propertyWillChange('totalExceptionCount');
        this.set('totalExceptionCount', oldCount + 1);
        this.propertyDidChange('totalExceptionCount');

        if (exception) {
            // if the exception is already stored, we just update the counter and
            // update the timestamp and info
            json['timestamp'] = date;

            exception.updateProperties(json);
        } else {
            // The exception is new, we add the exception to the collection
            json['count'] = 1;

            var newException = ExceptionsClient.store.createRecord(ExceptionsClient.Exception, json);
            newException.set('timestamp', date);
        }

        SC.RunLoop.end(); // We finish this run loop

        return YES;
    },


    // Precalculated properties

    // There should be other way of doing this using KVO and bindings
    // but the selection property gets cached.
    // All this section of the code *must* be refactored!

    testObserver: function() {
        if(ExceptionsClient.exceptionPage.backtraceView.updateLayer != undefined) {
            ExceptionsClient.exceptionPage.backtraceView.updateLayer();
        }
        if(ExceptionsClient.exceptionPage.sessionView.updateLayer != undefined) {
            ExceptionsClient.exceptionPage.sessionView.updateLayer();
        }
        if(ExceptionsClient.exceptionPage.environmentView.updateLayer != undefined) {
            ExceptionsClient.exceptionPage.environmentView.updateLayer();
        }
    }.observes('ExceptionsClient.exceptionsController.selection'),


    details: function() {
        var selection = this.get('selection');

        if(selection.get('length') == 0) {
            return "Nothing selected yet";
        } else {
            var exception = selection.get('firstObject');

            var html = "<h2>" + exception.get('identifier') + "<h2>";
            html = html + "<div id='ip'>" + exception.get('ip') + "</div>";

            return html
        }
    }.property('selection'),


    selectedIdentifier: function() {
        var selection = this.get('selection');
        if(selection.get('length') > 0) {
            return "<h1>" + selection.get('firstObject').get('identifier') + "</h1>";
        } else {
            return "";
        }
    }.property('selection'),

    selectedBacktrace: function() {
        var selection = this.get('selection');
        if(selection.get('length') > 0) {
            return selection.get('firstObject').get('backtrace');
        } else {
            return null;
        }
    }.property('selection'),

    selectedSession: function() {
        var selection = this.get('selection');
        if(selection.get('length') > 0) {
            return selection.get('firstObject').get('session');
        } else {
            return null;
        }
    }.property('selection'),

    selectedEnvironment: function() {
        var selection = this.get('selection');
        if(selection.get('length') > 0) {
            return selection.get('firstObject').get('environment');
        } else {
            return null;
        }
    }.property('selection'),

    selectedCount: function() {
        var selection = this.get('selection');
        if(selection.get('length') > 0) {
            return "<h1>" + selection.get('firstObject').get('count') + "</h1>";
        } else {
            return "";
        }
    }.property('selection'),


    selectedUrl: function() {
        var selection = this.get('selection');
        if(selection.get('length') > 0) {
            return selection.get('firstObject').get('url');
        } else {
            return "";
        }
    }.property('selection'),


    selectedIp: function() {
        var selection = this.get('selection');
        if(selection.get('length') > 0) {
            return selection.get('firstObject').get('ip');
        } else {
            return "";
        }
    }.property('selection'),


    updatedTimestamp: function() {
        var selection = this.get('selection');
        if(selection.get('length') > 0) {
            return selection.get('firstObject').get('timestamp');
        } else {
            return "";
        }
    }.property('selection'),


    connectionString: function() {
        return this.connectionUri;
    }.property('connectionUri'),


    // Socket connection


    // A new exception has been received
    newException: function(json) {
        ExceptionsClient.exceptionsController.addException(json);
    },


    // The connection to the Dispatcher has been closed
    websocketsConnectionClose: function(evt) {

    },


    // We store the connection
    setConnection: function(socket) {
        this.socket = socket;
    },


    // We return the connection
    getConnection: function() {
        return this.socket;
    }

}) ;
