-module (state_calculator).


-export ([calculate/1, milliseconds_now/0]).

-include ("game.hrl").

calculate(State) when is_record(State, user_state) ->
    TimeNow = milliseconds_now(),
    TimeDiff = TimeNow - State#user_state.last_update,
    Flower = State#user_state.flower,
    case (Flower#user_flower.time - TimeDiff) =< 0 of
        true ->
            NewFlower = Flower#user_flower{completed = true, time = 0},
            State#user_state{flower = NewFlower, last_update = TimeNow};
        _False ->
            State#user_state{last_update = TimeNow}
    end.

milliseconds_diff(T2, T1) ->
    timer:now_diff(T2, T1) div 1000.

milliseconds_now() ->
    {Mg, S, Mc} = now(),
    (Mg * 1000000000) + (S * 1000) + (Mc div 1000).
