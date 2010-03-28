// ==========================================================================
// Project:   ExceptionsClient - loginPage
// Copyright: ©2010 My Company, Inc.
// ==========================================================================
/*globals ExceptionsClient */

// This page describes the login user interface for your application.
ExceptionsClient.loginPage = SC.Page.design({

    mainPane: SC.PanelPane.design({

        layout: { width: 400, height: 575, centerX: 0, centerY: 0 },

        contentView: SC.View.design({


            childViews: "titleBar prompt addButton okButton controllerLabel routesList controllerField actionLabel actionField".w(),

            // PROMPT
            titleBar: SC.LabelView.design({
                layout: { top: 0, left: 0, height: 25, right: 0 },
                backgroundColor: '#778899',
                classNames: 'title-bar',
                textAlign: SC.ALIGN_LEFT,
                fontWeight: SC.BOLD_WEIGHT,
                value: "Exceptions Begone Dispatcher: new connection..."
            }),

            prompt: SC.LabelView.design({
                layout: { top: 47, left: 20, height: 28, right: 20 },
                value: "Build a list with exceptions you would like to be notified:"
            }),

            // INPUTS

            controllerLabel: SC.LabelView.design({
                layout: { top: 95, left: 20, width: 70, height: 18 },
                textAlign: SC.ALIGN_RIGHT,
                value: "Controller:"
            }),

            controllerField: SC.TextFieldView.design({
                layout: { top: 95, left: 100, height: 20, width: 270 },
                hint: "Rails controller",
                valueBinding: "ExceptionsClient.loginController.exceptionController"
            }),


            actionLabel: SC.LabelView.design({
                layout: { top: 123, left: 20, width: 70, height: 18 },
                textAlign: SC.ALIGN_RIGHT,
                value: "Action:"
            }),

            actionField: SC.TextFieldView.design({
                layout: { top: 123, left: 100, height: 20, width: 270 },
                hint: "Rails action",
                valueBinding: "ExceptionsClient.loginController.exceptionMethod"
            }),

            // List of exception routes

            routesList: SC.ScrollView.design({

                hasHorizontalScroller: NO,

                layout: { top: 203, left: 10, right: 10, height: 300 },
                backgroundColor: 'white',

                contentView: SC.ListView.design({

                    contentBinding: 'ExceptionsClient.exceptionRoutesController.arrangedObjects',
                    selectionBinding: 'ExceptionsClient.exceptionRoutesController.selection',
                    contentValueKey: "displayName",
                    canEditContent: NO,
                    canDeleteContent: YES,
                    rowHeight: 30
                })

            }),

            // buttons
            addButton: SC.ButtonView.design({
                layout: { top: 163, width: 90, height: 24, left: 280 },
                title: "Add",
                isDefault: YES,
                action: "ExceptionsClient.exceptionRoutesController.addExceptionRoute"
            }),

            okButton: SC.ButtonView.design({
                layout: { top: 525, width: 90, height: 24, centerX: 0, centerY: 0 },
                title: "Ready",
                isDefault: YES,
                action: "ExceptionsClient.loginController.doLogin"
            }),

            // login view

        })

//             childViews: 'titleView'.w(),

//             // --- titleView

//             titleView: SC.LabelView.design({
//                 //layout: {top: 200, left: 500, right: 500, height: 200},
//                 layout: { height: 200 },
//                 displayValue: "Exceptions Begone Dispatcher",
//                 fontWeight: SC.BOLD_WEIGHT,
//                 textAlign: 'left',
//                 isEditable: NO
//             }),

//             // --- routeFormView
//             //routeFormView: SC.View

//             // --- testButtonView
//             testButtonView: SC.ButtonView.design({

//                 layout: { top: 100, left: 100, right: 100, bottom: 100 },

//                 title: "Login here?",
//                 action: 'ExceptionsClient.loginController.doLogin'

//             })
        })

    })
//})


