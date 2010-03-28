// ==========================================================================
// Project:   ExceptionsClient - loadingPage
// Copyright: ©2010 My Company, Inc.
// ==========================================================================
/*globals ExceptionsClient */

// This page describes the loading user interface for your application.
ExceptionsClient.loadingPage = SC.Page.design({

    mainPane: SC.PanelPane.design({

        layout: { width: 400, height: 120, centerX: 0, centerY: 0 },

        contentView: SC.View.design({


            childViews: "spinnerView infoView".w(),

            spinnerView: SC.ProgressView.design({
                layout: { width: 360, height: 20, top: 20, left: 20, right:20 },
                isIndeterminate: YES,
                isRunning: YES
            }),

            infoView: SC.LabelView.design({
                layout: { width: 400, height: 100, top: 70, left: 0 },
                textAlign: SC.ALIGN_CENTER,
                value: "Connecting to the Exceptions Begone Dispatcher"
            })

        })


    })

})


