-module (user_session).

-export ([start/0, init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export ([plant_flower/0, get_flower_profit/0, buy_town/0, get_state/0, upgrade_castle/0]).

-include ("game.hrl").

-include_lib("eunit/include/eunit.hrl").

-behaviour (gen_server).

start() ->
  gen_server:start_link({local, user_session}, user_session, [], []).

init([]) ->
  {ok, #user_state{last_update=state_calculator:milliseconds_now(), flower=#user_flower{id=flower1}}}.


%% API

plant_flower() ->
    gen_server:call(user_session, plant_flower).

get_flower_profit() ->
    gen_server:call(user_session, get_flower_profit).

buy_town() ->
  gen_server:call(user_session, buy_town).

get_state() ->
  gen_server:call(user_session, get_state).

upgrade_castle() ->
  gen_server:call(user_session, upgrade_castle).

clean_plot() ->
  get_server:call(user_session, clean_plot).

%% Tests

get_state_test() ->
    {ok, _State=[{money, _Money}, {food, _Food}, {plot, _Flower}]} = get_state().

plant_flower_test() ->
  {ok, cleaned} = clean_plot(),
  {ok, planted} = plant_flower(),
  {ok, State} = get_state(),
  [{id, _FlowerId}, {time, _Time}, {completed, false}] = proplists:get_value(plot, State).

buy_tonw_test() ->
  {ok, [{money, Money}, {food, _Food}, {plot, _Flowers}]} = get_state(),
  {ok, bought} = buy_town(),
  {ok, [{money, NewMoney}, {food, _Food}, {plot, _Flower}]} = get_state(),
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
          {reply, {ok, planted}, NewState#user_state{flower=NewFlower}};
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

handle_call(upgrade_castle, _From, State) ->
  NewState = state_calculator:calculate(State),
  if
    State#user_state.level < ?MAX_LEVEL ->
      {reply, {ok, levelup}, NewState#user_state{level=NewState#user_state.level + 1}};
    true ->
      {reply, {error, max_level}, NewState}
  end;

handle_call(get_state, _From, State) ->
  NewState = state_calculator:calculate(State),
  Flower = NewState#user_state.flower,
  FlowerProplist = [{id, Flower#user_flower.id}, {time, Flower#user_flower.time}, {completed, Flower#user_flower.completed}],
  UserRespond = [{money, NewState#user_state.money}, {food, NewState#user_state.food}, {plot, FlowerProplist}],
  {reply, {ok, UserRespond}, NewState};

handle_call(clean_plot, _From, State) ->
  NewState = state_calculator:calculate(State),
  Plot = NewState#user_state.flower,
  {reply, {ok, cleaned}, NewState#user_state{flower=Plot#user_flower{completed = false, time = 0}}};

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

