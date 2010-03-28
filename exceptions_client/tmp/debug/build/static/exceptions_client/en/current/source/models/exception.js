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

    // TODO: Add your own code here.
    identifier: SC.Record.attr(String),
    url: SC.Record.attr(String),
    ip: SC.Record.attr(String),
    parameters: SC.Record.attr(Object),
    request_environment: SC.Record.attr(Object),
    session: SC.Record.attr(Object),
    environment: SC.Record.attr(Object),
    backtrace: SC.Record.attr(Array)
}) ;
; if ((typeof SC !== 'undefined') && SC && SC.scriptDidLoad) SC.scriptDidLoad('exceptions_client');