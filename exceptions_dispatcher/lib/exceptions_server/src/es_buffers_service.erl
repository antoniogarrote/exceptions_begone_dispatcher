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
        find/1,create_buffer/4, register_timer/0, extract_mails/1, send_summaries/1, build_summary/1]) .


%% public API


%% @doc
%% Starts the emongo application
start_mongodb() ->
    case application:get_env(exceptions_server,use_buffers_service) of
        {ok, true} ->
            esmtp:start(),
            timer:start(),
            register_timer(),
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

%% @doc
%% Register the timer function

register_timer() ->
    {{_Year,_Month,_Day},{Hour,Minutes,Seconds}} = erlang:localtime(),
    Secs = 59 - Seconds,
    Mins = 58 - case Minutes of
                    59     -> 0 ;
                    _Other -> Minutes
                end,
    Hours = 23 - Hour,
    Milsecs = timer:hms(Hours, Mins, Secs),
    timer:apply_after(Milsecs, ?MODULE, send_summaries,[]).

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

%% @doc
%% Creates a summary for a buffer
build_summary(Buffer) ->
    error_logger:info_msg("1"),
    Name = Buffer#exceptions_buffer.name,
    error_logger:info_msg("2"),
    Pid = es_buffers_service:find(Name),
    error_logger:info_msg("3"),
    Exceptions = es_buffer_process:all_exceptions(Pid),
    ExcepsJson = es_json:decode_json(Exceptions),
    lists:foldl(fun(E,Acum) ->
                        case es_json:is_exception(E) of
                            true  ->
                                Id = binary_to_list(es_json:get_identifier(E)),
                                Acum ++ Id ++ "\r\n" ;
                            false ->
                                Acum
                        end
                end,
                "Total exceptions: " ++ io_lib:format("~p",[length(ExcepsJson)]) ++ "\r\n\r\nList of exceptions :\r\n",
                ExcepsJson) .

%% @doc
%% Creates a summary for a buffer
extract_mails(Buffer) ->
    lists:map(fun(M) -> binary_to_list(M) end, Buffer#exceptions_buffer.mails) .


%% @doc
%% Send the summaries
send_summaries(_Args) ->
    Buffers = es_mongodb_utils:find_all_buffers(),
    lists:foreach(fun(B) ->
                          Mails = extract_mails(B),
                          Msg = build_summary(B),
                          lists:foreach(fun(Mail) ->
                                                esmtp:send(Mail,Msg)
                                        end,
                                        Mails)
                  end,
                  Buffers),
    register_timer() .
