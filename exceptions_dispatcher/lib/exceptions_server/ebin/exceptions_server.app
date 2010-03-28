%% This is the application resource file (.app file) for the exceptions_server,
%% application.
{application, exceptions_server,
  [{description, "Collecting and dispatching exceptions"},
   {vsn, "0.1.0"},
   {modules, [exceptions_server_app,
              exceptions_server_sup,
              exceptions_server_mochiweb_adapter,
              es_rabbit_backend,
              es_json]},
   {env, [{port, 8080},
          {docroot, "/Users/antonio.garrote/Desktop/www"}]},
   {registered,[exceptions_server_sup]},
   {applications, [kernel, stdlib]},
   {mod, {exceptions_server_app,[]}},
   {start_phases, []}]}.

