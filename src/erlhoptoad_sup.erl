-module(erlhoptoad_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Opts), {I, {I, start_link, Opts}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Hoptoad = ?CHILD(hoptoad, worker, ["production", "76fdb93ab2cf276ec080671a8b3d3866"]),
    {ok, { {one_for_one, 5, 10}, [Hoptoad]} }.
