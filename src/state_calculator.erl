-module (state_calculator).


-export ([calculate/1, milliseconds_now/0, milliseconds_diff/2]).

-include ("game.hrl").

calculate(State) when is_record(State, user_state) ->
    TimeNow = milliseconds_now(),
    TimeDiff = TimeNow - State#user_state.last_update,
    Flower = State#user_state.flower,
    State1 = calculate_level(State, TimeDiff),
    NewState = if
        Flower#user_flower.time > 0 andalso (Flower#user_flower.time - TimeDiff) =< 0 ->
            NewFlower = Flower#user_flower{completed = true, time = 0},
            State1#user_state{flower = NewFlower};
        true ->
            State1
    end,
    NewMoney = if
        NewState#user_state.money > 2000 ->
            NewState#user_state.money;
        true ->
            case TimeDiff div 50 of
                MoneyPlus when MoneyPlus + NewState#user_state.money < 2000 ->
                    NewState#user_state.money + MoneyPlus;
                _ ->
                    2000
            end
    end,
    NewState#user_state{last_update = TimeNow, money = NewMoney}.

calculate_level(State, TimeDiff) ->
    Level = State#user_state.level,
    if
        Level > 1 ->
            NewLevelTime = State#user_state.leveldown_time - TimeDiff,
            if
                NewLevelTime =< 0 ->
                    State#user_state{level = Level-1, leveldown_time = ?LEVEL_DOWN_TIMEOUT};
                true ->
                    State#user_state{leveldown_time = NewLevelTime}
            end;
        true -> State
    end.

milliseconds_diff(T2, T1) ->
    timer:now_diff(T2, T1) div 1000.

milliseconds_now() ->
    {Mg, S, Mc} = now(),
    (Mg * 1000000000) + (S * 1000) + (Mc div 1000).
