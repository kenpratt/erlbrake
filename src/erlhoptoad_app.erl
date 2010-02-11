-module(erlhoptoad_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Environment} = application:get_env(environment),
    {ok, ApiKey} = application:get_env(apikey),
    erlhoptoad_sup:start_link(Environment, ApiKey).

stop(_State) ->
    ok.
