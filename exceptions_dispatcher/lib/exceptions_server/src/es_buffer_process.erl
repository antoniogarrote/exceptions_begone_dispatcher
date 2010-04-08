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


-export([start_link/1, init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]) .


%% public API


%% @doc
%% Starts the service
start_link(BufferData) ->
    gen_server:start_link(?MODULE, [BufferData], []) .


%% callbacks

init([BufferData]) ->
    { ok, BufferData } .

%% dummy callbacks so no warning are shown at compile time

handle_call({create, _Options}, _From, State) ->
    {reply, ok, State } .

handle_cast(_Msg, State) ->
    {noreply, State} .

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_A,_B,_C) ->
    ok.

%% private API


