-module (state_calculator).


-export ([calculate/1, milliseconds_now/0]).

-include ("game.hrl").

calculate(State) when is_record(State, user_state) ->
    TimeDiff = milliseconds_diff(milliseconds_now(), State#user_state.last_update),
    io:format("time diff : ~p~n", [TimeDiff]),
    Flowers = State#user_state.flowers,
    AliveFlowers = [Flower || Flower <- Flowers, (Flower#user_flower.time - TimeDiff div 1000) > 0],
    CalculatedAliveFlowers = [Flower#user_flower{time = Flower#user_flower.time - TimeDiff} || Flower <- AliveFlowers],
    MoneyEarned = lists:foldl(
        fun (El, Sum) ->
            case lists:member(El, AliveFlowers) of
                false ->
                    Sum + get_flower_profit(El);
                _ ->
                    Sum
            end
        end
        , 0, Flowers),
    State#user_state{money = State#user_state.money + MoneyEarned, flowers = CalculatedAliveFlowers}.

get_flower_profit(#user_flower{id = FlowerId}) ->
    if
        FlowerId == ?flower1#flower.id ->
            ?flower1#flower.profit_money;
        FlowerId == ?flower2#flower.id ->
            ?flower2#flower.profit_money;
        true ->
            io:format("wrong flower id : ~p~n", [FlowerId]),
            0
    end.

milliseconds_diff(T2, T1) ->
    timer:now_diff(T2, T1) div 1000.

milliseconds_now() ->
    {Mg, S, Mc} = now(),
    (Mg * 1000000000) + (S * 1000) + (Mc div 1000).
