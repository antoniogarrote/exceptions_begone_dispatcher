%% This is the application resource file (.app file) for the exceptions_server,
%% application.
{application, exceptions_server,
  [{description, "Collecting and dispatching exceptions"},
   {vsn, "0.1.0"},
   {modules, [exceptions_server_app,
              exceptions_server_sup,
              exceptions_server_mochiweb_adapter,
              es_rabbit_backend,
              es_json,
              es_mongodb_utils,
              es_buffers_service]},
   {env, [{port, 8080},
          {docroot, "/Users/antonio.garrote/Desktop/www"},
            %% Should I start rabbitmq with this application or
            %% will I connect to a external rabbitmq server?
          {use_embedded_rabbit, true},
            %% If I'm connecting to an external rabbitmq server,
            %% this is the configuration I will use to connect.
            %% This configuration will be ignored if use_embedded_rabbit
            %% is set to true
          {rabbit_config, [{username, <<"guest">>},
                           {password, <<"guest">>},
                           {virtual_host, <<"/">>},
                           {host, "localhost"},
                           {port, 65535}]},
            %% Should I start the exceptions_agent service
          {use_buffers_service, true},
            %% MongoDB connection to use in the exceptions_agent service
          {mongodb_config, [{host, "localhost"},
                            {port, 27017},
                            {pool_size, 1}]}]},
   {registered,[exceptions_server_sup]},
   {applications, [kernel, stdlib]},
   {mod, {exceptions_server_app,[]}},
   {start_phases, []}]}.

