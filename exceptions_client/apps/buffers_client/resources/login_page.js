// ==========================================================================
// Project:   ExceptionsClient - loginPage
// Copyright: ©2010 My Company, Inc.
// ==========================================================================
/*globals ExceptionsClient */

// This page describes the login user interface for your application.
BuffersClient.loginPage = SC.Page.design({

    mainPane: SC.PanelPane.design({

        layout: { width: 400, height: 670, centerX: 0, centerY: 0 },

        contentView: SC.View.design({


            childViews: "titleBar promptLoad bufferNameLabel bufferNameField loadButton promptCreate bufferNameCreateLabel bufferNameCreateField \
                         controllerLabel controllerField actionLabel actionField addButton routesList mailLabel mailField addMailButton mailsList \
                         okButton capacityField capacityLabel".w(),

            /**************************************/
            // Load a buffer
            /**************************************/

            titleBar: SC.LabelView.design({
                layout: { top: 0, left: 0, height: 25, right: 0 },
                backgroundColor: '#778899',
                classNames: 'title-bar',
                textAlign: SC.ALIGN_LEFT,
                fontWeight: SC.BOLD_WEIGHT,
                value: "Exceptions Begone Buffers:"
            }),

            promptLoad: SC.LabelView.design({
                layout: { top: 47, left: 20, height: 28, right: 20 },
                value: "Load an existing buffer by name :"
            }),

            // INPUTS

            bufferNameLabel: SC.LabelView.design({
                layout: { top: 90, left: 20, width: 70, height: 18 },
                textAlign: SC.ALIGN_RIGHT,
                value: "Buffer name:"
            }),

            bufferNameField: SC.TextFieldView.design({
                layout: { top: 90, left: 100, height: 20, width: 270 },
                hint: "Name of the buffer",
                valueBinding: "BuffersClient.loginController.bufferName"
            }),

            loadButton: SC.ButtonView.design({
                layout: { top: 123, width: 90, height: 34, centerX: 0, centerY: 0 },
                title: "Load",
                isDefault: YES,
                action: "BuffersClient.loginController.doLoad"
            }),

            /**************************************/
            // Create buffer
            /**************************************/

            promptCreate: SC.LabelView.design({
                layout: { top: 165, left: 20, height: 28, right: 20 },
                value: "Create a new buffer:"
            }),

              // INPUTS

            bufferNameCreateLabel: SC.LabelView.design({
                layout: { top: 200, left: 20, width: 70, height: 18 },
                textAlign: SC.ALIGN_RIGHT,
                value: "Name:"
            }),

            bufferNameCreateField: SC.TextFieldView.design({
                layout: { top: 200, left: 100, height: 20, width: 270 },
                hint: "Name of the buffer",
                valueBinding: "BuffersClient.loginController.bufferCreateName"
            }),

            capacityLabel: SC.LabelView.design({
                layout: { top: 233, left: 20, width: 70, height: 18 },
                textAlign: SC.ALIGN_RIGHT,
                value: "Capacity:"
            }),

            capacityField: SC.TextFieldView.design({
                layout: { top: 233, left: 100, height: 20, width: 270 },
                hint: "Max number of items in the buffer",
                valueBinding: "BuffersClient.loginController.capacity"
            }),

            controllerLabel: SC.LabelView.design({
                layout: { top: 263, left: 20, width: 70, height: 18 },
                textAlign: SC.ALIGN_RIGHT,
                value: "Controller:"
            }),

            controllerField: SC.TextFieldView.design({
                layout: { top: 263, left: 100, height: 20, width: 270 },
                hint: "Rails controller",
                valueBinding: "BuffersClient.loginController.exceptionController"
            }),

            actionLabel: SC.LabelView.design({
                layout: { top: 290, left: 20, width: 70, height: 18 },
                textAlign: SC.ALIGN_RIGHT,
                value: "Action:"
            }),

            actionField: SC.TextFieldView.design({
                layout: { top: 290, left: 100, height: 20, width: 270 },
                hint: "Rails action",
                valueBinding: "BuffersClient.loginController.exceptionMethod"
            }),

            addButton: SC.ButtonView.design({
                layout: { top: 313, width: 140, height: 24, left: 225 },
                title: "Add exception",
                isDefault: YES,
                action: "BuffersClient.exceptionRoutesController.addExceptionRoute"
            }),


           // List of exception routes

            routesList: SC.ScrollView.design({

                hasHorizontalScroller: NO,

                layout: { top: 345, left: 30, right: 30, height: 100 },
                backgroundColor: 'white',

                contentView: SC.ListView.design({

                    contentBinding: 'BuffersClient.exceptionRoutesController.arrangedObjects',
                    selectionBinding: 'BuffersClient.exceptionRoutesController.selection',
                    contentValueKey: "displayName",
                    canEditContent: NO,
                    canDeleteContent: YES,
                    rowHeight: 30
                })

            }),

            mailLabel: SC.LabelView.design({
                layout: { top: 465, left: 20, width: 70, height: 18 },
                textAlign: SC.ALIGN_RIGHT,
                value: "Mail:"
            }),

            mailField: SC.TextFieldView.design({
                layout: { top: 465, left: 100, height: 20, width: 270 },
                hint: "email to be notified",
                valueBinding: "BuffersClient.loginController.email"
            }),

            addMailButton: SC.ButtonView.design({
                layout: { top: 490, width: 140, height: 24, left: 225 },
                title: "Add mail",
                isDefault: YES,
                action: "BuffersClient.emailsController.addEmail"
            }),

            mailsList: SC.ScrollView.design({

                hasHorizontalScroller: NO,

                layout: { top: 523, left: 30, right: 30, height: 100 },
                backgroundColor: 'white',

                contentView: SC.ListView.design({

                    contentBinding: 'BuffersClient.emailsController.arrangedObjects',
                    selectionBinding: 'BuffersClient.emailsController.selection',
                    contentValueKey: "email",
                    canEditContent: NO,
                    canDeleteContent: YES,
                    rowHeight: 30
                })

            }),


//             // buttons
            okButton: SC.ButtonView.design({
                layout: { top: 630, width: 90, height: 24, centerX: 0, centerY: 0 },
                title: "Ready",
                isDefault: YES,
                action: "BuffersClient.loginController.doCreate"
            })

        })

    })

})


