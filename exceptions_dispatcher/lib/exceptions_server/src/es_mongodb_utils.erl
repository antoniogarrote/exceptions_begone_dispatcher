%%%----------------------------------------------------------------
%%% @author Antonio Garrote Hernandez <antoniogarrote@gmail.com>
%%% @doc
%%%
%%% @end
%%% @copyright 2010 Antonio Garrote Hernandez
%%%----------------------------------------------------------------,
-module(es_mongodb_utils) .

-author("Antonio Garrote Hernandez") .

-include_lib("../include/buffers.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([start_service/0, create_buffer/4, find_all_buffers/0]) .

%% @doc
%% Starts the connection to MongoDB according to the configuration of the app.
start_service() ->
    {ok, Config} = application:get_env(exceptions_server,mongodb_config),
    Host = proplists:get_value(host, Config),
    Port = proplists:get_value(port, Config),
    PoolSize = proplists:get_value(pool_size, Config),
    application:start(emongo),
    emongo:add_pool(?CONFIG_POOL, Host, Port, ?CONFIG_DATABASE , PoolSize) .


%% @doc
%% Creates a new crapped collection with a certain name, size and max size per doc.
create_capped_collection(Pool, CollectionName, Size, Max) ->
    emongo:find_one(Pool, "$cmd", [{"create", CollectionName},{"capped",true},{"size",Size},{"max",Max}]) .


%% @doc
%% Creates a new buffer: a crapped collection for storing exceptions and the associated
%% meta data in the configuration database.
create_buffer(BufferName, Mails, Exceptions, Size) ->
    [[{<<"ok">>,1.0}]] = create_capped_collection(?CONFIG_POOL, BufferName, Size, 1024),
    emongo:insert(?CONFIG_POOL, "es_buffer_dbs", [{"name", BufferName},
                                                  {"database", BufferName},
                                                  {"mails", Mails},
                                                  {"exceptions", Exceptions},
                                                  {"inmediate", false}]) .

find_all_buffers() ->
    Data = emongo:find(?CONFIG_POOL, "es_buffer_dbs"),
    lists:map(fun(D) ->
                      buffer_from_mongo_to_record(D)
              end,
              Data).

buffer_from_mongo_to_record(Data) ->
    #exceptions_buffer{ name = binary_to_list(proplists:get_value(<<"name">>, Data)),
                        database = binary_to_list(proplists:get_value(<<"database">>, Data)),
                        mails = es_json:decode_json(binary_to_list(proplists:get_value(<<"mails">>, Data))),
                        exceptions = es_json:decode_json(binary_to_list(proplists:get_value(<<"exceptions">>, Data))),
                        inmediate = proplists:get_value(<<"inmediate">>,Data) } .


%% Tests

buffer_from_mongo_to_record_test() ->
    Orig = [{<<"_id">>,
             {oid,<<75,190,64,31,168,198,92,61,114,72,197,239>>}},
            {<<"name">>,<<"test2">>},
            {<<"database">>,<<"test2">>},
            {<<"mails">>,<<"[\"antoniogarrote@gmail.com\"]">>},
            {<<"exceptions">>,<<"[[\"*\",\"*\"]]">>},
            {<<"inmediate">>,false}],
    Result = buffer_from_mongo_to_record(Orig),
    ?assertEqual(Result,
                 {exceptions_buffer,"test2","test2",
                  [<<"antoniogarrote@gmail.com">>],
                  [[<<"*">>,<<"*">>]],
                  false}) .
