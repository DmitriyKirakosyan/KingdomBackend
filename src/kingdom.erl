%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc kingdom.

-module(kingdom).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the kingdom server.
start() ->
    kingdom_deps:ensure(),
    ensure_started(crypto),
    application:start(mongodb),
    application:start(kingdom).


%% @spec stop() -> ok
%% @doc Stop the kingdom server.
stop() ->
    application:stop(mongodb),
    application:stop(kingdom).
