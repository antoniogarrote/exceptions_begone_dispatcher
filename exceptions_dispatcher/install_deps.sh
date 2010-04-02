!#/bin/env bash

echo "****************************************************"
echo " Building Exceptions Begone Dispatcher dependencies "
echo "****************************************************"
echo ""

# echo "*** cleaning old builds"
# rm -rf rabbitmq-server
# rm -rf rabbitmq-codegen
# rm -rf rabbitmq-erlang-client

# echo "*** cloning rabbitmq-codegen"
# hg clone http://hg.rabbitmq.com/rabbitmq-codegen
# echo "*** cloning rabbitmq-server"
# hg clone http://hg.rabbitmq.com/rabbitmq-server
# echo "*** cloning rabbitmq-erlang-client"
# hg clone http://hg.rabbitmq.com/rabbitmq-erlang-client


cd lib/exceptions_server/deps

echo "*** Compiling rabbitmq-server"
cd rabbitmq-server
make PYTHON=`which python`

echo "*** Compiling rabbitmq-erlang-client"
cd .. && cd rabbitmq-erlang-client
make

echo "*** Compiling mochiweb"
cd .. && cd mochiweb-0.0.1
make


cd ../../../../

echo " *** finished"
