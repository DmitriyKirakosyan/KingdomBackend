-module (game_map).
-export ([save_map/1, save_objects/1, get_map/0, get_objects/0, add_town/3, remove_town/3]).

-define (map_collection, map).

-include ("game.hrl").

-define (NOTEST, true).
-include_lib("eunit/include/eunit.hrl").

%% API

save_map(Map) ->
    save_row(map, Map).

save_objects(Objects) ->
    save_row(objects, Objects).

get_map() ->
    get_row(map).

get_objects() ->
    get_row(objects).

add_town(Id, X, Y) ->
    Object = [{id, Id}, {x, X}, {y, Y}],
    NewObjects = case get_objects() of
        {ok, Objects} ->
            [Object | Objects];
        {error, wrong_map} ->
            [Object];
        _ ->
            [Object]
    end,
    save_objects(mochijson2:encode(NewObjects)).

remove_town(_Id, X, Y) ->
    NewObjects = case get_objects() of
        {ok, Objects} ->
            [Object || {struct, Object} <- Objects, proplists:get_value(<<"x">>, Object, 0) /= X orelse proplists:get_value(<<"y">>, Object, 0) /= Y];
        {error, wrong_map} ->
            []
    end,
    save_objects(mochijson2:encode(NewObjects)).

%% Internal functions

save_row(RowName, Row) ->
    case gamedb:find_one(?map_collection, {}) of
        {ok, [_Thing]} ->
            io:format("modify map [game_map.save_map]"),
            gamedb:modify(?map_collection, {}, {RowName, Row}),
            {ok, saved};
        {ok, []} ->
            io:format("insert map [game_map.save_map]"),
            gamedb:insert(?map_collection, {RowName, Row}),
            {ok, saved};
        _Error ->
            io:format("error : ~p~n", [_Error]),
            {error, smthg_wrong}
    end.

get_row(RowName) ->
    case gamedb:find_one(?map_collection, {}) of
        {ok, [DbMap]} ->
            io:format("db map : ~p~n", [DbMap]),
            case bson:lookup(RowName, DbMap) of
                {Json} ->
                    {ok, mochijson2:decode(Json)};
                _ ->
                    {error, wrong_map}
            end;
        _ ->
            {error, wrong_map}
    end.

remove_row() ->
    gamedb:remove_collection(?map_collection).

%% Tests

map_test() ->
    remove_row(),
    {error, wrong_map} = get_map(),
    save_map(mochijson2:encode([{width, 3}, {height, 4}])),
    {ok, {struct, [{<<"width">>, 3}, {<<"height">>, 4}]}} = get_map(),
    remove_row().

objects_test() ->
    remove_row(),
    {error, wrong_map} = get_objects(),
    save_objects(mochijson2:encode([[{id, <<"object1">>}, {<<"x">>, 2}, {<<"y">>, 2}]])),
    {ok, [{struct, [{<<"id">>, <<"object1">>}, {<<"x">>, 2}, {<<"y">>, 2}]}]} = get_objects(),
    remove_row().

add_town_test() ->
    remove_row(),
    {error, wrong_map} = get_objects(),
    add_town(<<"town1">>, 4, 1),
    {ok, [{struct, [{<<"id">>, <<"town1">>}, {<<"x">>, 4}, {<<"y">>, 1}]}]} = get_objects(),
    remove_row().


