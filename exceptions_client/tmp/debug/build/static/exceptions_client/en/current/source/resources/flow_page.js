// ==========================================================================
// Project:   ExceptionsClient - flowPage
// Copyright: ©2010 My Company, Inc.
// ==========================================================================
/*globals ExceptionsClient */

// This page describes the main user interface for your application.
ExceptionsClient.flowPage = SC.Page.create({
    mainView: SC.ScrollView.design({
        hasHorizontalScroller: NO,
        borderStyle: SC.BORDER_NONE,

        layout: { left:0, right:0, bottom:0, top: 12 },
        backgroundColor: 'red',
//        contentView: SC.ListView.design({
//        })
    })
});


; if ((typeof SC !== 'undefined') && SC && SC.scriptDidLoad) SC.scriptDidLoad('exceptions_client');