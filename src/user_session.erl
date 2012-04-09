-module (user_session).

-export ([start/0, init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export ([plant_flower/0, get_flower_profit/0, buy_town/0, get_state/0]).

-include ("game.hrl").

-include_lib("eunit/include/eunit.hrl").

-behaviour (gen_server).

start() ->
  gen_server:start_link({local, user_session}, user_session, [], []).

init([]) ->
  {ok, #user_state{last_update=state_calculator:milliseconds_now()}}.


%% API

plant_flower() ->
    gen_server:call(user_session, plant_flower).

get_flower_profit() ->
    gen_server:call(user_session, get_flower_profit).

buy_town() ->
  gen_server:call(user_session, buy_town).

get_state() ->
  gen_server:call(user_session, get_state).

%% Tests

get_state_test() ->
    {ok, _State=[{money, _Money}, {flower, Flower}]} = get_state().

plant_flower_test() ->
  {ok, added} = plant_flower(),
  {ok, State} = get_state(),
  [{id, _FlowerId}, {time, _Time}, {completed, false}] = proplists:get_value(flower, State).

buy_tonw_test() ->
  {ok, [{money, Money}, {flowers, _Flowers}]} = get_state(),
  {ok, bought} = buy_town(),
  {ok, [{money, NewMoney}, {flowers, _NewFlowers}]} = get_state(),
  MoneyDiff = Money - NewMoney,
  MoneyDiff = ?town_price.


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

handle_call(plant_flower, _From, State) ->
    NewState = state_calculator:calculate(State),
    Flower = NewState#user_state.flower,
    case Flower#user_flower.completed of
      false ->
          NewFlower = Flower#user_flower{time = ?flower1#flower.time_need},
          {reply, {ok, plated}, NewState#user_state{flower=NewFlower}};
      _True ->
        {reply, {error, not_completed}, NewState}
    end;

handle_call(get_flower_profit, _From, State) ->
    NewState = state_calculator:calculate(State),
    Flower = NewState#user_state.flower,
    case Flower#user_flower.completed of
        true ->
            NewFlower = Flower#user_flower{completed = false},
            {reply, {ok, profit_taken}, NewState#user_state{flower=NewFlower}};
        _False ->
            {reply, {error, not_completed}, NewState}
    end;

handle_call(get_state, _From, State) ->
  NewState = state_calculator:calculate(State),
  Flower = NewState#user_state.flower,
  FlowerProplist = [{id, Flower#user_flower.id}, {time, Flower#user_flower.time}, {completed, Flower#user_flower.completed}],
  UserRespond = [{money, NewState#user_state.money}, {flower, FlowerProplist}],
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

