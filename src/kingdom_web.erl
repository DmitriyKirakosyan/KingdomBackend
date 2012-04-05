%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for kingdom.

-module(kingdom_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    io:format("~p~n", [Path]),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
                    "hello" ->
                        gamedb:insert(test, {x, 3});
                    _ ->
                        Req:serve_file(Path, DocRoot)
                end;
            'POST' ->
                io:format("post coming...~n", []),
                ParsedPost = Req:parse_post(),
                Request = proplists:get_value("request", ParsedPost),
                Response = case mochijson2:decode(Request) of
                   {struct, Props} ->
                       handle_request(Props);
                   _ ->
                      io:format("bad request : ~p~n", [Request]),
                      {error, bad_request}
                end,
                Req:respond({200, mochiweb_headers:empty(), mochijson2:encode([{response, [Response]}])});

            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

handle_request(Request) ->
  io:format("parsed from json request : ~p~n", [Request]),
  user_request_handler:handle(Request).

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
