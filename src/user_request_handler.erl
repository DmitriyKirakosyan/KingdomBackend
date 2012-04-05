-module (user_request_handler).

-export ([handle/1]).

handle(Request) ->
    RequestName = proplists:get_value(<<"request">>, Request),
    case RequestName of
        <<"getmap">> ->
            io:format("get map request"),
            ok;
        <<"savemap">> ->
            io:format("save map request"),
            ok
    end.