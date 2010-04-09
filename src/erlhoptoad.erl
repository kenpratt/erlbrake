-module(erlhoptoad).

-export([start/0, stop/0]).

%% @spec start() -> ok
%% @doc Start erlhoptoad.
start() ->
    ensure_started(sasl),
    ensure_started(ibrowse),
    application:start(erlhoptoad).

%% @spec stop() -> ok
%% @doc Stop erlhoptoad.
stop() ->
    application:stop(erlhoptoad),
    application:stop(ibrowse),
    application:stop(sasl).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
