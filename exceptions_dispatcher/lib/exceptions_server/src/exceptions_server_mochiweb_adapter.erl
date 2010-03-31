%%%----------------------------------------------------------------
%%% @author Antonio Garrote Hernandez <antoniogarrote@gmail.com>
%%% @doc
%%%
%%% @end
%%% @copyright 2010 Antonio Garrote Hernandez
%%%----------------------------------------------------------------,
-module(exceptions_server_mochiweb_adapter) .

-author("Antonio Garrote Hernandez") .

-behaviour(gen_server) .

-include_lib("eunit/include/eunit.hrl").


-export([start_link/0, loop/2]) .
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, handle_potential_client/3]).


-define(STAR, <<"*">>).
-define(EXCEPTION,<<"exception">>).
-define(DOT,<<".">>).
-define(SHARP,<<"#">>).

%% Public API


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []) .


%% Callbacks


init({}) ->
    {ok, Path} = application:get_env(exceptions_server,docroot),
    {ok, Port} = application:get_env(exceptions_server,port),
    error_logger:info_msg("Starting mochiweb adapter with DOCROOT ~p and PORT ~p", [Path, Port]),
    Loop = fun (Req) ->
                   error_logger:info_msg("User mochiweb main loop", []),
		   ?MODULE:loop(Req, Path)
	   end,
    Params = [{name, "TheExceptionsServer"}, {loop, Loop}, {port, Port}],
    error_logger:info_msg("here we go", []),
    Res = mochiweb_http:start(Params),
    error_logger:info_msg("here we ok? ~p", [Res]),
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, ok, State} .

handle_cast({handle_request, Request, DocRoot}, State) ->
    error_logger:info_msg("request???"),
    spawn(?MODULE,
          handle_request,
          [Request, DocRoot]),
    {noreply, State} .


%% dummy callbacks so no warning are shown at compile time
handle_info(_Msg, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State} .


terminate(shutdown, _State) ->
    mochiweb_http:stop(?MODULE),
    ok.



%% Private functions


%% @doc
%% Handles a mochiweb incoming request
handle_request(Req, DocRoot) ->
    error_logger:info_msg("received web request: ~p", [Req]),
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of

                "test" ->
                    Req:respond({200,
                                 [{"Content-Type", "text/plain"},
                                  {"Charset", "UTF-8"}],
                                 "Hola mundo!"}) ;

                "exceptions" ->
                    case parse_ws_upgrade(Req) of
                        false ->
                            error_logger:info_msg("Not a valid web socket upgrade request"),
                            Req:respond({401,
                                         [{"Content-Type", "text/plain"},
                                          {"Charset", "UTF-8"}],
                                         "invalid request"}) ;
                        Sock ->
                            CRes = gen_tcp:controlling_process(Sock,self()),
                            error_logger:info_msg("changed process ? ~p",[CRes]),
                            error_logger:info_msg("Sock (~p)~n", [Sock]),
                            case gen_tcp:recv(Sock, 0) of
                                {ok, Message} ->
                                    error_logger:info_msg("Receiving message (~p).~n", [Message]),
                                    Json = es_json:decode_json(Message),
                                    error_logger:info_msg("Decoded: ~p",[Json]),

                                    % Generate keys
                                    Keys = transform_keys(Json),

                                    % Declare queues
                                    lists:foreach(fun(Key) ->
                                                          QueueName = io_lib:format("queue~p~p",[Key,self()]),
                                                          es_rabbit_backend:create_queue(list_to_binary(QueueName),Key)
                                                  end,
                                                  Keys),

                                    % Creates a new consumer process for the keys
                                    MyPid = self(),
                                    Fun = fun(Data) ->
                                                  MyPid ! {self(), Data} ,
                                                  receive
                                                      Something -> Something
                                                  end
                                          end,

                                    es_rabbit_backend:consume(Fun,Keys),

                                    % Client main loop:
                                    % Wait for data from the queue and then push it to
                                    % the client through the socket until the socket
                                    % get closed
                                    push_data(Sock) ;

                                {error, Reason} ->
                                    error_logger:info_msg("Cannot receive (~p)~n", [Reason]),
                                    error
                            end
                    end ;
                _ ->  PathP = case Path == [] of
                                  true  -> "index.html" ;
                                  false -> case lists:nth(1,lists:reverse(Path)) == $/ of
                                               true  -> Path ++ "index.html" ;
                                               false -> Path
                                           end
                              end ,
                      Req:serve_file(lists:nthtail(1,"/" ++ PathP), DocRoot)
            end;

        'POST' ->
            case match_project_path(Path) of
                nomatch ->
                    error_logger:info_msg("No hay match"),
                    Req:not_found() ;

                _Match ->
                    error_logger:info_msg("received exception: ~p", [Req:parse_post()]),
                    try
                        Star = ?STAR,
                        Exception = ?EXCEPTION,
                        Dot = ?DOT,
                        Data = get(mochiweb_request_body),
                        error_logger:info_msg("received exception parsed: ~p", [Data]),
                        case es_json:process_json_message(Data) of
                            {exception, C, M, JsonStr} ->  Keys = [<<Exception/binary,Dot/binary,C/binary,Dot/binary,M/binary>>],
                                                           es_rabbit_backend:publish(JsonStr, Keys) ;
                            {notification, C, JsonStr } -> Keys = [<<C/binary,Dot/binary,Star/binary,Dot/binary,Star/binary>>],
                                                           es_rabbit_backend:publish(JsonStr, Keys) ;
                            _ -> dont_care
                        end,
                        Req:respond({201,
                                     [{"Content-Type", "text/plain"},
                                      {"Charset", "UTF-8"}],
                                     "created"})
                    catch _Exception ->
                       Req:respond({501,
                                    [{"Content-Type", "text/plain"},
                                     {"Charset", "UTF-8"}],
                                    "error parsing request"})
                    end
            end;
        _ ->
            Req:not_found()
    end.

%% The loop function
loop(Req, DocRoot) ->
    handle_request(Req, DocRoot) .


%% The client main loop
push_data(Socket) ->
    receive
        {From, SomeExceptionData} ->
              % we try to push the data
            case ws_send(Socket, SomeExceptionData) of
                ok               -> From ! ok,
                                    push_data(Socket);
                {error, _Reason} -> From ! exit % The socket is close we can stop the consumer
            end
    end .

%% Handle an incoming request that my be a valid WebSockets client
handle_potential_client(Req,Socket,no_handshake) ->
    receive
        continue -> % The server thread has changed ownership in the socket
            case parse_ws_upgrade(Req) of
                false ->
                    error_logger:info_msg("Not a valid web socket upgrade request"),
                    Req:respond({401,
                                 [{"Content-Type", "text/plain"},
                                  {"Charset", "UTF-8"}],
                                 "invalid request"}) ;
                true ->
                    error_logger:info_msg("valid web socket client, let's read the exception's subscriptions"),
                    Result = ws_read(Socket),
                    error_logger:info_msg("Read: ~p",[Result]),
                    Json = es_json:decode_json(Result),
                    error_logger:info_msg("Decoded: ~p",[Json]),
                    gen_tcp:close(Socket)
            end
    end .

parse_ws_upgrade(Request) ->
    case header(Request,"upgrade") of
        "WebSocket" -> error_logger:info_msg("Found WebSocket upgrade request",[]),
                       Socket = (Request):get(socket),
                       ResSetOpts = inet:setopts(Socket, [{keepalive, true}]),
                       error_logger:info_msg("Result of setting keepalive for socket ~p",[ResSetOpts]),
                       gen_tcp:send(Socket, ws_handshake(Request)),
                       Socket ;
        _Other      -> false
    end.

ws_handshake(Request) ->
    Path = Request:get(path),
    WSOrigin = Request:get_header_value("origin"),
    WSHost = Request:get_header_value("host"),
    WSLocation = lists:flatten([WSHost, Path]),
    error_logger:info_msg("HANDSHAKING TO  ~p",[WSOrigin]),
    [
     "HTTP/1.1 101 Web Socket Protocol Handshake\r\n",
     "Upgrade: WebSocket\r\n",
     "Connection: Upgrade\r\n",
     "WebSocket-Origin: ", " ", WSOrigin, "\r\n",
     "WebSocket-Location: ",
     "  ws://", WSLocation, "\r\n\r\n"
    ] .


ws_send(Socket, Data) ->
    %Res = "tmp",
    Res = gen_tcp:send(Socket, [0,Data,255]),
    error_logger:info_msg("Result of writing through websocket: ~p",[Res]),
    Res .

ws_read(Sock) ->
    error_logger:info_msg("Sock (~p)~n", [Sock]),

    receive
        {tcp, _S, Message} ->
            error_logger:info_msg("Receiving message (~p).~n", [Message]),
            Message;
        Other ->
            error_logger:info_msg("Cannot receive (~p)~n", [Other]),
            error
    end.

%% @doc
%% returns the header from the mochiweb request.
%% @type key() = atom() | binary() | string().
header(Req,Key) ->
    (Req):get_header_value(Key).



transform_keys(Json) ->
    Exception = ?EXCEPTION,
    Dot = ?DOT,
    lists:map(fun([C,M]) ->
                <<Exception/binary,Dot/binary,C/binary,Dot/binary,M/binary>>
              end,
              Json).

match_project_path(Path) ->
    {ok,RE} = re:compile("projects\/.*\/notifications"),
    re:run(Path,RE) .

%%
%% Test
%%


match_path_test() ->
    Res1 = match_project_path("projects/test/notifications"),
    ?assertEqual(Res1,{match,[{0,27}]}),
    Res2 = match_project_path("projects_wrong/test/notifications"),
    ?assertEqual(Res2,nomatch) .


transform_keys_test() ->
    Json = [[<<"a">>,<<"b">>]],
    Keys = transform_keys(Json),
    ?assertEqual([<<"exception.a.b">>],Keys) .
