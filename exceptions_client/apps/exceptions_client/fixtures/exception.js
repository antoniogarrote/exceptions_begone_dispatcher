// ==========================================================================
// Project:   ExceptionsClient.Exception Fixtures
// Copyright: ©2010 My Company, Inc.
// ==========================================================================
/*globals ExceptionsClient */

sc_require('models/exception');

ExceptionsClient.Exception.FIXTURES = [

    { guid: 1,
      identifier: "Base#core (UnexpectedError)",
      parameters: {"arg1": "test1",
                   "arg2": "test2" },
      url: "http://test.com/exception1",
      ip: "127.0.0.1",
      request_environment: {"val1": "val1",
                            "val2": "val2" },
      session: {"ses1": "val1",
                "ses2": "val2" },
      environment: {"env1": "val1",
                    "env2": "val2" },
      backtrace: ["stack1","stack2","stack3"] },

    { guid: 2,
      identifier: "Base#core2 (UnexpectedError)",
      parameters: {"arg1": "test1",
                   "arg2": "test2" },
      url: "http://test.com/exception1",
      ip: "127.0.0.1",
      request_environment: {"val1": "val1",
                            "val2": "val2" },
      session: {"ses1": "val1",
                "ses2": "val2" },
      environment: {"env1": "val1",
                    "env2": "val2" },
      backtrace: ["stack1","stack2","stack3"] }
];
