// ==========================================================================
// Project:   ExceptionsClient - exceptionPage
// Copyright: ©2010 My Company, Inc.
// ==========================================================================
/*globals ExceptionsClient */
sc_require('views/property_enumeration');
sc_require('views/backtrace');

// This page describes the main user interface for your application.
ExceptionsClient.exceptionPage = SC.Page.create({
    summaryView: SC.View.design({
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
    }),

    environmentView: ExceptionsClient.PropertyEnumerationView.design({
        titleContent: 'Environment',
        backgroundColor: 'gray',
        classNames: ['tab-overflow'],
        propertyToEnumerate: 'environment',
        valueBinding: 'ExceptionsClient.exceptionsController.selectedEnvironment'
    }),

    sessionView: ExceptionsClient.PropertyEnumerationView.design({
        titleContent: 'Session',
        classNames: ['tab-overflow'],
        backgroundColor: 'gray',
        propertyToEnumerate: 'session',
        valueBinding: 'ExceptionsClient.exceptionsController.selectedSession'
    }),

    backtraceView: ExceptionsClient.BacktraceView.design({
        backgroundColor: 'gray',
        classNames: ['tab-overflow'],
        valueBinding: 'ExceptionsClient.exceptionsController.selectedBacktrace'
    })
});
