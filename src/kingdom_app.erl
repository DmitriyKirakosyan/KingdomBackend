%% @author Mochi Media <dev@mochimedia.com>
%% @copyright kingdom Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the kingdom application.

-module(kingdom_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for kingdom.
start(_Type, _StartArgs) ->
    kingdom_deps:ensure(),
    kingdom_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for kingdom.
stop(_State) ->
    ok.
