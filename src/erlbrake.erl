-module(erlbrake).

-export([start/0, stop/0]).

%% @spec start() -> ok
%% @doc Start erlbrake.
start() ->
    ensure_started(sasl),
    ensure_started(ibrowse),
    application:start(erlbrake).

%% @spec stop() -> ok
%% @doc Stop erlbrake.
stop() ->
    application:stop(erlbrake),
    application:stop(ibrowse),
    application:stop(sasl).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
