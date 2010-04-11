// ==========================================================================
// Project:   BuffersClient
// Copyright: ©2010 My Company, Inc.
// ==========================================================================
/*globals BuffersClient */

// This is the function that will start your app running.  The default
// implementation will load any fixtures you have created then instantiate
// your controllers and awake the elements on your page.
//
// As you develop your application you will probably want to override this.
// See comments for some pointers on what to do next.
//

sc_require('models/login_information');

BuffersClient.main = function main() {

    // Step 1: Instantiate Your Views
    // The default code here will make the mainPane for your application visible
    // on screen.  If you app gets any level of complexity, you will probably
    // create multiple pages and panes.
    BuffersClient.getPath('loginPage.mainPane').append() ;

    var loginInformation = BuffersClient.LoginInformation.create({ exceptionController: '*', exceptionMethod: '*', bufferName: '', bufferCreateName: '', email: 'test@test.com', capacity: "5000" });
    BuffersClient.loginController.set('content', loginInformation);

    var exceptionRoutes = BuffersClient.store.find(BuffersClient.ExceptionRoute);
    BuffersClient.exceptionRoutesController.set('content',exceptionRoutes);

    var query = SC.Query.local(BuffersClient.Exception, { orderBy: 'timestamp DESC' });
    var exceptions = BuffersClient.store.find(query);
    BuffersClient.exceptionsController.set('content', exceptions);

    var emails = BuffersClient.store.find(BuffersClient.Email);
    BuffersClient.emailsController.set('content',emails);

} ;

function main() { BuffersClient.main(); }
