#!/bin/bash

echo " *** Cleaning beam files"
rm lib/exceptions_server/ebin/*.beam

echo " *** Compiling beam files"
cd lib/exceptions_server/src

erlc -D EUNIT -pa ebin -pa deps/rabbitmq-server/ebin -pa deps/rabbitmq-erlang-client/ebin -pa deps/mochiweb/ebin -W -I../include +warn_unused_vars +warn_unused_import -o ../ebin es_json.erl
erlc -D EUNIT -pa ebin -pa deps/rabbitmq-server/ebin -pa deps/rabbitmq-erlang-client/ebin -pa deps/mochiweb/ebin -W -I../include +warn_unused_vars +warn_unused_import -o ../ebin es_rabbit_backend.erl
erlc -D EUNIT -pa ebin -pa deps/rabbitmq-server/ebin -pa deps/rabbitmq-erlang-client/ebin -pa deps/mochiweb/ebin -W -I../include +warn_unused_vars +warn_unused_import -o ../ebin es_buffers_service.erl
erlc -D EUNIT -pa ebin -pa deps/rabbitmq-server/ebin -pa deps/rabbitmq-erlang-client/ebin -pa deps/mochiweb/ebin -W -I../include +warn_unused_vars +warn_unused_import -o ../ebin es_buffer_process.erl
erlc -D EUNIT -pa ebin -pa deps/rabbitmq-server/ebin -pa deps/rabbitmq-erlang-client/ebin -pa deps/mochiweb/ebin -W -I../include +warn_unused_vars +warn_unused_import -o ../ebin exceptions_server_mochiweb_adapter.erl
erlc -D EUNIT -pa ebin -pa deps/rabbitmq-server/ebin -pa deps/rabbitmq-erlang-client/ebin -pa deps/mochiweb/ebin -W -I../include +warn_unused_vars +warn_unused_import -o ../ebin es_mongodb_utils.erl
erlc -D EUNIT -pa ebin -pa deps/rabbitmq-server/ebin -pa deps/rabbitmq-erlang-client/ebin -pa deps/mochiweb/ebin -W -I../include +warn_unused_vars +warn_unused_import -o ../ebin exceptions_server_app.erl
erlc -D EUNIT -pa ebin -pa deps/rabbitmq-server/ebin -pa deps/rabbitmq-erlang-client/ebin -pa deps/mochiweb/ebin -W -I../include +warn_unused_vars +warn_unused_import -o ../ebin exceptions_server_sup.erl

echo " *** Finsihed"
