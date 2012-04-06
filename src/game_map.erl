-module (game_map).
-export ([save_map/1, get_map/0]).

-define (map_collection, map).

save_map(Map) ->
    case gamedb:find_one(?map_collection, {}) of
        {ok, [_Thing]} ->
            io:format("modify map [game_map.save_map]"),
            gamedb:modify(?map_collection, {}, {map, Map}),
            {ok, saved};
        {ok, []} ->
            io:format("insert map [game_map.save_map]"),
            gamedb:insert(?map_collection, {map, Map}),
            {ok, saved};
        _Error ->
            io:format("error : ~p~n", [_Error]),
            {error, smthg_wrong}
    end.

get_map() ->
    case gamedb:find_one(?map_collection, {}) of
        {ok, [DbMap]} ->
            io:format("db map : ~p~n", [DbMap]),
            case bson:lookup(map, DbMap) of
                {JsonMap} ->
                    {ok, mochijson2:decode(JsonMap)};
                _ ->
                    {error, wrong_map}
            end;
        _ ->
            {error, wrong_map}
    end.