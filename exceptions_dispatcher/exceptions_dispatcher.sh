#!/bin/bash
erl -pa lib/exceptions_server/ebin -pa lib/exceptions_server/deps/rabbitmq-server/ebin -pa lib/exceptions_server/deps/rabbitmq-erlang-client/ebin -pa lib/exceptions_server/deps/mochiweb-0.0.1/ebin -pa lib/exceptions_server/deps/emongo/ebin -pa lib/exceptions_server/deps/esmtp/ebin -s exceptions_server_app shell_start
