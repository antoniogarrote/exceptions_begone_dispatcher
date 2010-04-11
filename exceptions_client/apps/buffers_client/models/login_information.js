// ==========================================================================
// Project:   BuffersClient.LoginInformation
// Copyright: ©2010 My Company, Inc.
// ==========================================================================
/*globals BuffersClient */

/** @class

  (Document your Model here)

  @extends SC.Record
  @version 0.1
*/
BuffersClient.LoginInformation = SC.Record.extend(
/** @scope BuffersClient.LoginInformation.prototype */ {

    exceptionController: SC.Record.attr(String),
    exceptionMethod: SC.Record.attr(String),
    bufferName: SC.Record.attr(String),
    capacity: SC.Record.attr(String),
    bufferCreateName: SC.Record.attr(String),
    email: SC.Record.attr(String),

    displayName: function() {
        return this.get('exceptionController') + "#" + this.get('exceptionMethod') ;
    }.property('exceptionController', 'exceptionMethod'),

    arrayValue: function() {
        return [ this.get('exceptionController'), this.get('exceptionMethod') ] ;
    }

}) ;
