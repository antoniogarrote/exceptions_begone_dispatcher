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

    // CRUD operations

    addException: function(data) {

        var json = data.notification.payload;
        json['identifier'] = data.notification.identifier;

        var exceptions = this.get('content') || SC.Array.create();

        var exception = exceptions.find(function(item, index, enumerable) {
            return item.get('identifier') == json['identifier'];
        });

        var date = SC.DateTime.create();

        this.totalExceptionCount += 1;
        ExceptionsClient.exceptionsController.notifyPropertyChange('exceptionsCountProperty');


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

        return YES;
    },


    // Precalculated properties


    // There should be other way of doing this using KVO and bindings
    // but the selection property gets cached.
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


    exceptionsCountProperty: function() {
        return this.totalExceptionCount;
    }.property('totalExceptionCount'),


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
