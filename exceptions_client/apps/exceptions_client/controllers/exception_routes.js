// ==========================================================================
// Project:   ExceptionsClient.exceptionRoutesController
// Copyright: ©2010 My Company, Inc.
// ==========================================================================
/*globals ExceptionsClient */

/** @class

  (Document Your Controller Here)

  @extends SC.ArrayController
*/
  ExceptionsClient.exceptionRoutesController = SC.ArrayController.create(
      SC.CollectionViewDelegate,
      /** @scope ExceptionsClient.exceptionRoutesController.prototype */ {

          collectionViewDeleteContent: function(view, content, indexes) {

              var records = indexes.map(function(idx) {
                  return this.objectAt(idx);
              }, this);

              records.invoke('destroy');

              var selIndex = indexes.get('min')-1;
              if (selIndex<0) selIndex = 0;
              this.selectObject(this.objectAt(selIndex));
          },

          addExceptionRoute: function() {

              var controllerRoute = ExceptionsClient.loginController.get('content').get('exceptionController');
              var actionRoute = ExceptionsClient.loginController.get('content').get('exceptionMethod');

              if(controllerRoute == "" || actionRoute == "") {
                  return NO;
              } else {
                  var routes = this.get('content');
                  var alreadyInList = routes.find(function(item, index, enumerable) {
                      return item.get('exceptionController') == controllerRoute && item.get('exceptionMethod') == actionRoute
                  })

                  if(alreadyInList != null) {
                      return NO;
                  }
              }

              // create a new task in the store
              var route = ExceptionsClient.store.createRecord(ExceptionsClient.ExceptionRoute, {
                  'exceptionController': controllerRoute,
                  'exceptionMethod': actionRoute
              });

              // select new task in UI
              this.selectObject(route);

              return YES;
          },

          listOfRoutes: function() {
              var routes = this.get('content').map(function(item, index, enumerable) {
                  return item.arrayValue();
              });
              return routes;
          }

      }) ;
