-module (game_map).
-export ([save_map/1, get_map/0]).

-define (map_collection, map).

save_map(Map) ->
    case gamedb:find_one(?map_collection, {}) of
        [_Thing] ->
            gamedb:modify(?map_collection, {}, {map, Map}),
            {ok, saved};
        [] ->
            gamedb:insert(?map_collection, {map, Map}),
            {ok, saved};
        _ ->
            {error, smthg_wrong}
    end.

get_map() ->
    case gamedb:find_one(?map_collection, {}) of
        [DbMap] ->
            case bson:lookup(DbMap, map) of
                {JsonMap} ->
                    {ok, JsonMap};
                _ ->
                    {error, wrong_map}
            end;
        _ ->
            {error, wrong_map}
    end.