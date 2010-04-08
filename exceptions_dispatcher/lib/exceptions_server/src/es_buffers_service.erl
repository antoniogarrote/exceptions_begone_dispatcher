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


-export([start_mongodb/0, start_link/0, init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]) .


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
%% Starts the service
start_link() ->
    start_mongodb() ,
    gen_server:start_link({local, es_buffers_service}, es_buffers_service, [], []) .


%% callbacks

init(_Arguments) ->
    Buffers = es_mongodb_utils:find_all_buffers(),
    BufferProcesses = lists:map(fun(B) -> {ok,Pid} = es_buffer_process:start_link(B),
                                          {#exceptions_buffer.name, Pid}
                                end,
                                Buffers),
    { ok, BufferProcesses } .

%% dummy callbacks so no warning are shown at compile time

handle_call({create, _Options}, _From, State) ->
    {reply, ok, State } .

handle_cast(_Msg, State) ->
    {noreply, State} .

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_A, _B, _C) ->
    ok.

%% private API


