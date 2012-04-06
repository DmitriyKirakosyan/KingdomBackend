-module (user_request_handler).

-export ([handle/2]).

handle(<<"getmap">>, _Params) ->
    game_map:get_map();
handle(<<"savemap">>, Params) ->
    case proplists:get_value(<<"map">>, Params) of
        {struct, MapParams} ->
            io:format("json encoded map : ~p~n", [mochijson2:encode(MapParams)]),
            game_map:save_map(mochijson2:encode(MapParams));
        _ -> {error, wrong_map}
    end.