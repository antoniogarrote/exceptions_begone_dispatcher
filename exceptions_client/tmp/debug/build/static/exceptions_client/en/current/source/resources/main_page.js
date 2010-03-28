// ==========================================================================
// Project:   ExceptionsClient - mainPage
// Copyright: ©2010 My Company, Inc.
// ==========================================================================
/*globals ExceptionsClient */

// This page describes the main user interface for your application.
ExceptionsClient.mainPage = SC.Page.design({

  // The main pane is made visible on screen as soon as your app is loaded.
  // Add childViews to this pane for views to display immediately on page
  // load.
  mainPane: SC.MainPane.design({

      childViews: 'middleView topView bottomView'.w(),

      topView: SC.ToolbarView.design({
          layout: { top: 0, left: 0, right: 0, height: 36 },
          nanchorLocation: SC.ANCHOR_TOP
      }),


      middleView: SC.SplitView.design({
          layout: { top:36, left:12, right:12, bottom:32 },
          classNames: ['collections-tab'],
          layoutDirection: SC.LAYOUT_VERTICAL,
          defaultThickness: 0.5,
          autoresizeBehavior: SC.RESIZE_BOTTOM_RIGHT,
          canCollapseViews: YES,

          topLeftMinThickness: 200,
          topLeftMaxThickness: 900,
          topLeftView: SC.ListView.design({
              layout: { top:0, left:0, right:0, bottom:0 },
              backgroundColor: 'white',
              contentBinding: 'ExceptionsClient.exceptionsController.arrangedObjects',
              selectionBinding: 'ExceptionsClient.exceptionsController.selection',
              contentValueKey: "identifier",
              rowHeight: 31
          }),

          dividerView: SC.SplitDividerView,

          bottomRightView: SC.LabelView.design({
              backgroundColor: 'lightgray',
              valueBinding: "ExceptionsClient.exceptionsController.details"
          })
      }),

      bottomView: SC.ToolbarView.design({
          layout: { bottom: 0, left: 0, right: 0, height: 32 },
          anchorLocation: SC.ANCHOR_BOTTOM
      })

  })

});


; if ((typeof SC !== 'undefined') && SC && SC.scriptDidLoad) SC.scriptDidLoad('exceptions_client');