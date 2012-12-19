-module(erlbrake).

-export([start/0, stop/0]).

%% @spec start() -> ok
%% @doc Start erlbrake.
start() ->
    start_ok(erlbrake).
    
start_ok(App) ->
    start_ok(App, application:start(App, permanent)).

start_ok(_App, ok) -> ok;
start_ok(_App, {error, {already_started, _App}}) -> ok;
start_ok(App, {error, {not_started, Dep}}) ->
    ok = start_ok(Dep),
    start_ok(App);
start_ok(App, {error, Reason}) ->
    erlang:error({erlbrake_start_failed, App, Reason}).

%% @spec stop() -> ok
%% @doc Stop erlbrake.
stop() ->
    application:stop(erlbrake).

