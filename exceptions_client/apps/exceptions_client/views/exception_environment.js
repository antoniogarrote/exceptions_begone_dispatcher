// ==========================================================================
// Project:   ExceptionsClient.ExceptionEnvironmentView
// Copyright: ©2010 My Company, Inc.
// ==========================================================================
/*globals ExceptionsClient */

/** @class

  (Document Your View Here)

  @extends SC.View
*/
  ExceptionsClient.ExceptionEnvironmentView = SC.View.extend({
      /** @scope ExceptionsClient.ExceptionListView.prototype */

      classNames: ['exception-environment-view'],

      render: function(context, firstTime) {
          var selection = this.get('value');

          if(selection != undefined && selection.get('length') > 0) {
              var exception = selection.get('firstObject');
              var environment = exception.get('environment');

              context = context.begin().push('Environment').end();
              context = context.begin('ul');
              for(var v in environment) {
                  context = context.begin('li').begin('span').addClass('prop-key').push(v).push(':').end('span').push(environment[v]).end('li');
              }
              context = context.end('ul');
          }
      }

  });
