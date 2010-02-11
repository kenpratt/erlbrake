-module(erlhoptoad_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Opts), {I, {I, start_link, Opts}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Environment, ApiKey) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Environment, ApiKey]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Environment, ApiKey]) ->
    Hoptoad = ?CHILD(hoptoad, worker, [Environment, ApiKey]),
    {ok, { {one_for_one, 5, 10}, [Hoptoad]} }.
