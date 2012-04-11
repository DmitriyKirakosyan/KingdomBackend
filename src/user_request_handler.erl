-module (user_request_handler).

-export ([handle/2, handle_other/3]).

handle(<<"get_map">>, _Params) ->
    game_map:get_map();
handle(<<"save_map">>, Params) ->
    case proplists:get_value(<<"map">>, Params) of
        {struct, MapParams} ->
            io:format("json encoded map : ~p~n", [mochijson2:encode(MapParams)]),
            game_map:save_map(mochijson2:encode(MapParams));
        _ -> {error, wrong_map}
    end;

handle(<<"get_objects">>, _Params) ->
    game_map:get_objects();

handle(<<"plant_flower">>, _Params) ->
    user_session:plant_flower();

handle(<<"get_flower_profit">>, Params) ->
    user_session:get_flower_profit();

handle(<<"buy_town">>, Params) ->
    case user_session:buy_town() of
        {ok, bought} = BoughtResult->
            Id = proplists:get_value(<<"id">>, Params, 0),
            X = proplists:get_value(<<"x">>, Params, 0),
            Y = proplists:get_value(<<"y">>, Params, 0),
            game_map:add_town(Id, X, Y),
            BoughtResult;
        _Error ->
            {error, no_money}
    end;

handle(<<"remove_town">>, Params) ->
    Id = proplists:get_value(<<"id">>, Params, 0),
    X = proplists:get_value(<<"x">>, Params, 0),
    Y = proplists:get_value(<<"y">>, Params, 0),
    game_map:remove_town(Id, X, Y),
    {ok, removed};

handle(<<"get_state">>, _Params) ->
    user_session:get_state().


%% other requests

handle_other(<<"save_feedback">>, GameRequestType, Request) ->
    GameName = case GameRequestType of
        <<"hunter_request">> -> hunter;
        <<"spaceshooter_request">> -> spaceshooter;
        <<"tank_request">> -> tank;
        _ -> ninja
    end,
    case proplists:get_value(<<"feedback">>, Request) of
        undefined -> {error, empty_feedback};
        Feedback ->
            gamedb:insert(minigame_feedback, {game_name, GameName, feedback, Feedback})
    end.