// ==========================================================================
// Project:   ExceptionsClient.Exception
// Copyright: ©2010 My Company, Inc.
// ==========================================================================
/*globals ExceptionsClient */

/** @class

  (Document your Model here)

  @extends SC.Record
  @version 0.1
*/
ExceptionsClient.Exception = SC.Record.extend(
/** @scope ExceptionsClient.Exception.prototype */ {

    // Properties
    identifier: SC.Record.attr(String),
    url: SC.Record.attr(String),
    ip: SC.Record.attr(String),
    parameters: SC.Record.attr(Object),
    request_environment: SC.Record.attr(Object),
    session: SC.Record.attr(Object),
    environment: SC.Record.attr(Object),
    backtrace: SC.Record.attr(Array),
    timestamp: SC.Record.attr(SC.DateTime, { format: '%d/%m/%Y %H:%M:%S' }),
    count: SC.Record.attr(Number),

    updateProperties: function(Json) {
        this.set('ip', Json['ip']);
        this.set('parameters', Json['parameters']);
        this.set('environment',Json['environment']);
        this.set('backtrace',Json['backtrace']);
        this.set('timestamp',Json['timestamp']);
        this.set('count', this.get('count') + 1);
    },

    display: function() {
        var data = "[ " + this.get('count') + " ]" ;
        data += "     " + this.get('identifier');
        data += " @ " + this.get('url');
        data += ", (" + this.get('timestamp') + ")";

        return data;
    }.property('identifier', 'timestamp', 'count', 'url')

}) ;
