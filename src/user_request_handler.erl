-module (user_request_handler).

-export ([handle/1]).

handle(Request) ->
    RequestName = proplists:get_value(<<"request">>, Request),
    case RequestName of
        <<"getmap">> ->
            game_map:get_map();
        <<"savemap">> ->
            case proplists:get_value(<<"map">>, Request) of
                Map when is_binary(Map) ->
                    game_map:save_map(Map);
                _ -> {error, wrong_map}
            end
    end.