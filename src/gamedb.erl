-module(gamedb).
-export ([start/0]).
-export ([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export ([insert/2, find/2]).

-behaviour(gen_server).

-record (state, {conn}).
-define (DB_NAME, kingdom).

start() ->
  gen_server:start_link({local, gamedb}, gamedb, [], []).

init([]) ->
  {ok, Conn} = mongo:connect({localhost, 27017}),
  {ok, #state{conn=Conn}}.


insert(Collection, Data) ->
    gen_server:call(gamedb, {insert, Collection, Data}).

find(Collection, {}) ->
    gen_server:call(gamedb, {find, Collection, {}}).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({insert, Collection, Data}, _From, State) ->
    mongo:do(safe, master, State#state.conn, ?DB_NAME, fun() ->
        mongo:insert(Collection, Data)
    end),
    {reply, ok, State};
handle_call({find, Collection, {}}, _From, State) ->
    Cursor = mongo:do(safe, master, State#state.conn, ?DB_NAME, fun() ->
                mongo:find(Collection, {})
    end),
    Result = mongo:next(Cursor),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    mongo:disconnect(State#state.conn).

