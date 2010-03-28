%%%----------------------------------------------------------------
%%% @author  Antonio Garrote <antoniogarrote@gmail.com>
%%% @doc
%%% @end
%%% @copyright 2010 Antonio Garrote
%%%----------------------------------------------------------------
-module(exceptions_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    Webserver  = {exceptions_server_mochiweb_adapter, {exceptions_server_mochiweb_adapter, start_link, []},
                  Restart, Shutdown, Type, [exceptions_server_mochiweb_adapter]},

    RabbitBackend  = {es_rabbit_backend, {es_rabbit_backend, start_link, []},
                      Restart, Shutdown, Type, [es_rabbit_backend]},


    {ok, {SupFlags, [Webserver, RabbitBackend]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


