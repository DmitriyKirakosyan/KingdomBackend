-module (user_session).

-export ([start/0, init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export ([add_flower/1, buy_town/0, get_state/0]).

-include ("game.hrl").

-behaviour (gen_server).

start() ->
  gen_server:start_link({local, user_session}, user_session, [], []).

init([]) ->
  {ok, #user_state{}}.


%% API

add_flower(FlowerId) when is_record(FlowerId, flower) ->
    gen_server:call(user_session, {add_flower, FlowerId}).

buy_town() ->
  gen_server:call(user_session, buy_town).

get_state() ->
  gen_server:call(user_session, get_state).


%% Internal functions

handle_call(buy_town, _From, State) ->
    NewState = state_calculator:calculate(State),
    UserMoney = NewState#user_state.money,
    case UserMoney - ?town_price of
      NewMoney when NewMoney > 0 ->
        {reply, {ok, bought}, NewState#user_state{money=NewMoney}};
      _ ->
        {reply, {error, not_enough_money}, NewState}
    end;

handle_call({add_flower, FlowerId}, _From, State) ->
    NewState = state_calculator:calculate(State),
    NewFlowers = case FlowerId of
      flower2 -> [#user_flower{id=?flower2#flower.id, time=?flower2#flower.time_need} | State#user_state.flowers];
      flower1 -> [#user_flower{id=?flower1#flower.id, time=?flower1#flower.time_need} | State#user_state.flowers];
      _ -> NewState#user_state.flowers
    end,
    {reply, {ok, added}, NewState#user_state{flowers=NewFlowers}};

handle_call(get_state, _From, State) ->
  NewState = state_calculator:calculate(State),
  FlowerList = [[{id, Flower#user_flower.id}, {time, Flower#user_flower.time}] || Flower <- NewState#user_state.flowers],
  UserRespond = [{money, NewState#user_state.money}, {flowers, FlowerList}],
  {reply, {ok, UserRespond}, NewState};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
   ok.

