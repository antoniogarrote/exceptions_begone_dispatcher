// ==========================================================================
// Project:   BuffersClient.ExceptionListView
// Copyright: ©2010 My Company, Inc.
// ==========================================================================
/*globals BuffersClient */

/** @class

  (Document Your View Here)

  @extends SC.View
*/
  BuffersClient.ExceptionsListItemView = SC.ListItemView.extend({
      /** @scope BuffersClient.ExceptionListView.prototype */

      classNames: ['exceptions-list-item-view'],

      contentDisplayProperties: 'count timestamp identifier url'.w(),

      render: function(context, firstTime) {

          var content = this.get('content');
          var count = content.get('count');
          var timestamp = content.get('timestamp');
          var identifier = content.get('identifier');
          var url = content.get('url');

          context = context.begin().addClass('exception-count').push(' %@ '.fmt(count)).end();
          context = context.begin().addClass('exception-text');
          context = context.begin('span').addClass('exception-identifier').push(identifier).end('span');
          context = context.push(' @ ');
          context = context.push(url);
          context = context.push(', ');
          context = context.push('(%@)'.fmt(timestamp));
          context = context.end();
      }

  });
