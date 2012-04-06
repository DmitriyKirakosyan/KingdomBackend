-module (user_session).

-export ([init/0]).

-behaviour (gen_server).

-record (state, {conn}).
-define (DB_NAME, kingdom).

start() ->
  gen_server:start_link({local, gamedb}, gamedb, [], []).

init([]) ->
  {ok, Conn} = mongo:connect({localhost, 27017}),
  {ok, #state{conn=Conn}}.


%% API

insert(Collection, Data) ->
    gen_server:call(gamedb, {insert, Collection, Data}).

modify(Collection, Selector, Data) ->
    gen_server:call(gamedb, {modify, Collection, Selector, Data}).

find_all(Collection, Selector) ->
    gen_server:call(gamedb, {find_all, Collection, Selector}).
find_one(Collection, Selector) ->
    gen_server:call(gamedb, {find_one, Collection, Selector}).

%% Internal functions

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, State) ->
    mongo:disconnect(State#state.conn).

