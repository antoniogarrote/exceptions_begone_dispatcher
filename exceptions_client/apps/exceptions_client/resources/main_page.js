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
          childViews: 'titleLabelView connectionLabelView'.w(),
          layout: { top: 0, left: 0, right: 0, height: 50 },
          nanchorLocation: SC.ANCHOR_TOP,

//           logoImageView: SC.ImageView.design({
//               layout: { top: 10, left: 10, height:49, width:89 },
//               value: "/exceptions_client/images/logo.png"
//           }),

          titleLabelView: SC.LabelView.design({
              layout: { top: 10, left: 10},
              value: "Exceptions Begone Dispatcher"
          }),

          connectionLabelView: SC.LabelView.design({
              layout: { top: 10, right: 12, height:30 },
              valueBinding: 'ExceptionsClient.exceptionsController.connectionString',
              textAlign: SC.ALIGN_RIGHT
           })
      }),


      middleView: SC.SplitView.design({
          layout: { top:50, left:0, right:0, bottom:32 },
          classNames: ['collections-tab'],
          layoutDirection: SC.LAYOUT_VERTICAL,
          defaultThickness: 0.5,
          autoresizeBehavior: SC.RESIZE_BOTTOM_RIGHT,
          canCollapseViews: YES,

          topLeftMinThickness: 200,
          topLeftMaxThickness: 900,
          topLeftView: SC.ListView.design({
              exampleView: ExceptionsClient.ExceptionsListItemView,
              layout: { top:0, left:0, right:0, bottom:0 },
              contentBinding: 'ExceptionsClient.exceptionsController.arrangedObjects',
              selectionBinding: 'ExceptionsClient.exceptionsController.selection',
              rowHeight: 31,
              showAlternatingRows: YES
          }),

          dividerView: SC.SplitDividerView,

          bottomRightView: SC.TabView.design({

              value: 'ExceptionsClient.exceptionPage.summaryView',

              items: [
                  { title: 'Summary', value: 'ExceptionsClient.exceptionPage.summaryView' },
                  { title: 'Session', value: 'ExceptionsClient.exceptionPage.sessionView' },
                  { title: 'Environment', value: 'ExceptionsClient.exceptionPage.environmentView' },
                  { title: 'Backtrace', value: 'ExceptionsClient.exceptionPage.backtraceView' }
              ],

              itemTitleKey: 'title',
              itemValueKey: 'value',

              layout: { top: 0, left: 0, right: 0 },

              userDefaultKey: "mainPane"
          })
      }),

      bottomView: SC.ToolbarView.design({
          childViews: 'countLabel countView'.w(),

          layout: { bottom: 0, left: 0, right: 0, height: 32 },
          anchorLocation: SC.ANCHOR_BOTTOM,

          countLabel: SC.LabelView.design({
              layout: { bottom: 5, right: 50, height:20 },
              value: 'total exceptions:',
              textAlign: SC.ALIGN_RIGHT
          }),

          countView: SC.LabelView.design({
              layout: { bottom: 3, right: 12, height:20 },
              valueBinding: 'ExceptionsClient.exceptionsController.totalExceptionCount',
              fontWeight: SC.BOLD_WEIGHT,
              textAlign: SC.ALIGN_RIGHT
           })
      })

  })

});


