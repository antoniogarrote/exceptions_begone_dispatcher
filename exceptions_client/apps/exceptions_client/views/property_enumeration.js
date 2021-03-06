// ==========================================================================
// Project:   ExceptionsClient.PropertyEnumerationView
// Copyright: ©2010 My Company, Inc.
// ==========================================================================
/*globals ExceptionsClient */

/** @class

  (Document Your View Here)

  @extends SC.View
*/
  ExceptionsClient.PropertyEnumerationView = SC.View.extend({
      /** @scope ExceptionsClient.ExceptionListView.prototype */

      classNames: ['exception-environment-view'],

      propertyToEnumerate: '',
      titleContent: '',

      render: function(context, firstTime) {
          var selection = ExceptionsClient.exceptionsController.get('selection');

          if(selection != undefined && selection.get('length') > 0) {
              var exception = selection.get('firstObject');
              var environment = exception.get(this.get('propertyToEnumerate'));

              context = context.begin().addClass('property-view-title').push(this.get('titleContent')).end();
              context = context.begin('ul');
              for(var v in environment) {
                  context = context.begin('li').addClass('prop-key').begin('span').push(v).push(':').end('span').push(environment[v]).end('li');
              }
              context = context.end('ul');
          }
      }

  });
