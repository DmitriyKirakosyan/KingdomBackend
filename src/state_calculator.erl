-module (state_calculator).


-export ([calculate/1, milliseconds_now/0, milliseconds_diff/2]).

-include ("game.hrl").

calculate(State) when is_record(State, user_state) ->
    TimeNow = milliseconds_now(),
    TimeDiff = TimeNow - State#user_state.last_update,
    Flower = State#user_state.flower,
    NewState = calculate_level(State, TimeDiff),
    if
        Flower#user_flower.time > 0 andalso (Flower#user_flower.time - TimeDiff) =< 0 ->
            NewFlower = Flower#user_flower{completed = true, time = 0},
            NewState#user_state{flower = NewFlower, last_update = TimeNow};
        true ->
            NewState#user_state{last_update = TimeNow}
    end.

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
