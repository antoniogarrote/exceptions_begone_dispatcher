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

  // TODO: Add your own code here.

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
    }.property('selection').cacheable()


}) ;
; if ((typeof SC !== 'undefined') && SC && SC.scriptDidLoad) SC.scriptDidLoad('exceptions_client');