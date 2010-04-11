%%%----------------------------------------------------------------
%%% @author Antonio Garrote Hernandez <antoniogarrote@gmail.com>
%%% @doc
%%%
%%% @end
%%% @copyright 2010 Antonio Garrote Hernandez
%%%----------------------------------------------------------------,
-module(es_buffers_service) .

-author("Antonio Garrote Hernandez") .

-behaviour(gen_server) .

-include_lib("../include/buffers.hrl").
-include_lib("eunit/include/eunit.hrl").


-export([start_mongodb/0, start_link/0, init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3,
        find/1,create_buffer/4, test_state/0]) .


%% public API


%% @doc
%% Starts the emongo application
start_mongodb() ->
    case application:get_env(exceptions_server,use_buffers_service) of
        {ok, true} ->
            es_mongodb_utils:start_service();
        {ok, false}  ->
            dont_care
    end .

%% @doc
%% Finds a buffer process provided the name of the buffer
find(Name) when is_binary(Name) ->
    find(binary_to_list(Name)) ;
find(Name) ->
    gen_server:call(es_buffers_service, {find,Name}) .

%% @doc
%% Create a new buffer
create_buffer(Name,Capacity, Exceptions, Mails) ->
    gen_server:call(es_buffers_service, {create_buffer,Name, Capacity, Exceptions, Mails}) .

test_state() ->
    gen_server:call(es_buffers_service, {state}) .

%% @doc
%% Starts the service
start_link() ->
    start_mongodb() ,
    gen_server:start_link({local, es_buffers_service}, es_buffers_service, [], []) .


%% callbacks

init(_Arguments) ->
    Buffers = es_mongodb_utils:find_all_buffers(),
    BufferProcesses = lists:map(fun(B) -> {ok,Pid} = es_buffer_process:start_link(B),
                                          {B#exceptions_buffer.name, Pid}
                                end,
                                Buffers),
    { ok, BufferProcesses } .

%% dummy callbacks so no warning are shown at compile time

handle_call({create_buffer,Name, Capacity, Exceptions, Mails}, _From, State) ->
    es_mongodb_utils:create_buffer(Name,Mails,Exceptions,Capacity),
    {reply, ok, State } ;
handle_call({find, Name}, _From, State) ->
    Res = case proplists:get_value(Name,State) of
              undefined  -> undefined ;
              Pid  -> Pid
          end,
    {reply, Res, State} ;
handle_call({state}, _From, State) ->
    {reply, State, State}.

handle_cast(_Msg, State) ->
    {noreply, State} .

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_A, _B, _C) ->
    ok.

%% private API


