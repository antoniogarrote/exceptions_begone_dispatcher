// ==========================================================================
// Project:   ExceptionsClient.ExceptionRoute
// Copyright: ©2010 My Company, Inc.
// ==========================================================================
/*globals ExceptionsClient */

/** @class

  (Document your Model here)

  @extends SC.Record
  @version 0.1
*/
ExceptionsClient.ExceptionRoute = SC.Record.extend(
/** @scope ExceptionsClient.ExceptionRoute.prototype */ {

    exceptionController: SC.Record.attr(String),
    exceptionMethod: SC.Record.attr(String),

    displayName: function() {
        return this.get('exceptionController') + "#" + this.get('exceptionMethod') ;
    }.property('exceptionController', 'exceptionMethod'),

    arrayValue: function() {
        return [ this.get('exceptionController'), this.get('exceptionMethod') ] ;
    }
}) ;
