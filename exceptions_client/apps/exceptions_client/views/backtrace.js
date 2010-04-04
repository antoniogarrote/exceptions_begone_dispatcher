// ==========================================================================
// Project:   ExceptionsClient.BacktraceView
// Copyright: ©2010 My Company, Inc.
// ==========================================================================
/*globals ExceptionsClient */

/** @class

  (Document Your View Here)

  @extends SC.View
*/
  ExceptionsClient.BacktraceView = SC.View.extend({
      /** @scope ExceptionsClient.ExceptionListView.prototype */

      classNames: ['exception-environment-view'],


      render: function(context, firstTime) {
          var selection = ExceptionsClient.exceptionsController.get('selection');

          if(selection != undefined && selection.get('length') > 0) {
              var exception = selection.get('firstObject');
              var backtraces = exception.get('backtrace');

              console.log("rendering backtrace");

             if(backtraces != null) {
                 context = context.begin().addClass('backtrace-title').push('Backtrace').end();
                 context = context.begin('div').addClass('backtraces');
                 for(var i=0; i<backtraces.length; i++) {
                     var trace = backtraces[i];
                     context = context.begin().addClass('backtrace').push(trace).end();
                 }
                 context = context.end('div');
             } else {
                 context = context.push("");
             }
          }
      }

  });
