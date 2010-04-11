// ==========================================================================
// Project:   BuffersClient - mainPage
// Copyright: ©2010 My Company, Inc.
// ==========================================================================
/*globals BuffersClient */

// This page describes the main user interface for your application.
BuffersClient.mainPage = SC.Page.design({

  // The main pane is made visible on screen as soon as your app is loaded.
  // Add childViews to this pane for views to display immediately on page
  // load.
  mainPane: SC.MainPane.design({

      childViews: 'middleView topView bottomView'.w(),

      topView: SC.ToolbarView.design({
          childViews: 'titleLabelView'.w(),
          layout: { top: 0, left: 0, right: 0, height: 50 },
          nanchorLocation: SC.ANCHOR_TOP,

//           logoImageView: SC.ImageView.design({
//               layout: { top: 10, left: 10, height:49, width:89 },
//               value: "/exceptions_client/images/logo.png"
//           }),

          titleLabelView: SC.LabelView.design({
              layout: { top: 10, left: 10},
              value: "Exceptions Begone Buffers"
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
              exampleView: BuffersClient.ExceptionsListItemView,
              layout: { top:0, left:0, right:0, bottom:0 },
              contentBinding: 'BuffersClient.exceptionsController.arrangedObjects',
              selectionBinding: 'BuffersClient.exceptionsController.selection',
              rowHeight: 31,
              showAlternatingRows: YES
          }),

          dividerView: SC.SplitDividerView,

          bottomRightView: SC.TabView.design({

              value: 'BuffersClient.exceptionPage.summaryView',

              items: [
                  { title: 'Summary', value: 'BuffersClient.exceptionPage.summaryView' },
                  { title: 'Session', value: 'BuffersClient.exceptionPage.sessionView' },
                  { title: 'Environment', value: 'BuffersClient.exceptionPage.environmentView' },
                  { title: 'Backtrace', value: 'BuffersClient.exceptionPage.backtraceView' }
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
              valueBinding: 'BuffersClient.exceptionsController.totalExceptionCount',
              fontWeight: SC.BOLD_WEIGHT,
              textAlign: SC.ALIGN_RIGHT
           })
      })

  })

});


