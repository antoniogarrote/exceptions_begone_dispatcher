// ==========================================================================
// Project:   BuffersClient.emailsController
// Copyright: ©2010 My Company, Inc.
// ==========================================================================
/*globals BuffersClient */

/** @class

  (Document Your Controller Here)

  @extends SC.ArrayController
*/
  BuffersClient.emailsController = SC.ArrayController.create(
      SC.CollectionViewDelegate,
      /** @scope BuffersClient.emailsController.prototype */ {

          collectionViewDeleteContent: function(view, content, indexes) {

              var records = indexes.map(function(idx) {
                  return this.objectAt(idx);
              }, this);

              records.invoke('destroy');

              var selIndex = indexes.get('min')-1;
              if (selIndex<0) selIndex = 0;
              this.selectObject(this.objectAt(selIndex));
          },

          addEmail: function() {

              var emailAddress = BuffersClient.loginController.get('content').get('email');

              if(emailAddress == "") {
                  return NO;
              } else {
                  var emails = this.get('content');
                  var alreadyInList = emails.find(function(item, index, enumerable) {
                      return item.get('email') == emailAddress;
                  })

                  if(alreadyInList != null) {
                      return NO;
                  }
              }

              // create a new task in the store
              var email = BuffersClient.store.createRecord(BuffersClient.Email, {
                  'email': emailAddress
              });

              // select new task in UI
              this.selectObject(email);

              return YES;
          },

          listOfEmails: function() {
              var emails = this.get('content').map(function(item, index, enumerable) {
                  return item.get('email');
              });
              return emails;
          }

      }) ;
