%%%----------------------------------------------------------------
%%% @author Antonio Garrote Hernandez <antoniogarrote@gmail.com>
%%% @doc
%%%
%%% @end
%%% @copyright 2010 Antonio Garrote Hernandez
%%%----------------------------------------------------------------,
-module(es_buffer_process) .

-author("Antonio Garrote Hernandez") .

-behaviour(gen_server) .

-include_lib("../include/buffers.hrl").
-include_lib("eunit/include/eunit.hrl").


-export([start_link/1, init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3,
         all_exceptions/1, create_exception/2]) .


%% public API


%% @doc
%% Starts the service
start_link(BufferData) ->
    gen_server:start_link(?MODULE, [BufferData], []) .

all_exceptions(Pid) ->
    gen_server:call(Pid,{exceptions}) .

create_exception(Pid,Exception) ->
    gen_server:call(Pid, {exception, Exception}) .

%% callbacks

init([BufferData]) ->
    % Create a mongodb pool
    Pool = list_to_atom(BufferData#exceptions_buffer.name),
    es_mongodb_utils:start_pool(Pool,?EXCEPTIONS_DATABASE),

    % Generate keys
    Keys = es_json:transform_keys(BufferData#exceptions_buffer.exceptions),

    % Declare queues
    lists:foreach(fun(Key) ->
                          QueueName = io_lib:format("bufferqueue~p~p",[Key,self()]),
                          es_rabbit_backend:create_queue(list_to_binary(QueueName),Key)
                  end,
                  Keys),

    % Creates a new consumer process for the keys
    MyPid = self(),
    Fun = fun(Data) ->
                  gen_server:call(MyPid,{exception, Data})
          end,

    es_rabbit_backend:consume(Fun,Keys),
    { ok, {Pool,BufferData} } .

%% dummy callbacks so no warnings are shown at compile time

handle_call({exception, ExceptionData}, _From, {Pool,BufferData}) ->
    Collection = BufferData#exceptions_buffer.database,
    emongo:insert(Pool,Collection,[{data,ExceptionData}]),
    { reply, ok, {Pool,BufferData} };
handle_call({exceptions}, _From, {Pool, #exceptions_buffer{database = Collection}} = State) ->
    Exceptions = emongo:find_all(Pool, Collection),
    Json = lists:map(fun(E) -> proplists:get_value(<<"data">>, E) end, Exceptions),
    { reply, es_json:binaries_to_json_array(Json), State }.

handle_cast(_Msg, State) ->
    {noreply, State} .

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_A,_B,_C) ->
    ok.

%% private API


