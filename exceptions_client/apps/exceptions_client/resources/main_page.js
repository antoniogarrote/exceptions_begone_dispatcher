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
              //contentValueKey: "display",
              rowHeight: 31,
              showAlternatingRows: YES
          }),

          dividerView: SC.SplitDividerView,

          bottomRightView: SC.View.design({
              childViews: "counterView identifierView urlLabel urlView ipLabel ipView dateLabel dateView".w(),

              backgroundColor: 'gray',

              // Views
              counterView: SC.LabelView.design({
                  layout: {top: 10, left: 10, height: 50, width: 30},
                  valueBinding: 'ExceptionsClient.exceptionsController.selectedCount',
                  escapeHTML: NO,
                  textAlign: SC.ALIGN_RIGHT
              }),

              identifierView: SC.LabelView.design({
                  layout: {top: 10, left: 50, height: 50, right: 50},
                  valueBinding: 'ExceptionsClient.exceptionsController.selectedIdentifier',
                  escapeHTML: NO
              }),

              urlLabel: SC.LabelView.design({
                  layout: {top: 100, left: 20, height: 50, width: 60},
                  value: 'URL:',
                  fontWeight: SC.BOLD_WEIGHT,
                  textAlign: SC.ALIGN_RIGHT
              }),

              urlView: SC.LabelView.design({
                  layout: {top: 100, left: 90, height: 50, right: 20},
                  valueBinding: 'ExceptionsClient.exceptionsController.selectedUrl'
              }),

              ipLabel: SC.LabelView.design({
                  layout: {top: 130, left: 20, height: 50, width: 60},
                  value: 'IP:',
                  fontWeight: SC.BOLD_WEIGHT,
                  textAlign: SC.ALIGN_RIGHT
              }),

              ipView: SC.LabelView.design({
                  layout: {top: 130, left: 90, height: 50, right: 20},
                  valueBinding: 'ExceptionsClient.exceptionsController.selectedIp'
              }),

              dateLabel: SC.LabelView.design({
                  layout: {top: 160, left: 20, height: 50, width: 60},
                  value: 'Received:',
                  fontWeight: SC.BOLD_WEIGHT,
                  textAlign: SC.ALIGN_RIGHT
              }),

              dateView: SC.LabelView.design({
                  layout: {top: 160, left: 90, height: 50, right: 20},
                  valueBinding: 'ExceptionsClient.exceptionsController.updatedTimestamp'
              })

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
              valueBinding: 'ExceptionsClient.exceptionsController.exceptionsCountProperty',
              fontWeight: SC.BOLD_WEIGHT,
              textAlign: SC.ALIGN_RIGHT
           })
      })

  })

});


