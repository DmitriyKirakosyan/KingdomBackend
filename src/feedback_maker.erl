-module (feedback_maker).

-export ([make/0]).

-define (feedback_collection, minigame_feedback).

make() ->
    NinjaBallFeedback = get_feedback_from_db(ninja),
    StoneAgeHunter = get_feedback_from_db(hunter),
    TankDraft = get_feedback_from_db(tanks),
    <<" <html> <h2><b>NinjaBall</b></h1> <br/>",NinjaBallFeedback/binary," <br/> <h2><b>StoneAgeHunter</b></h2><br/>",
        StoneAgeHunter/binary, "<br/> <h2><b>TankDraft</b></h2><br/>",TankDraft/binary," </html>">>.


get_feedback_from_db(GameName) ->
case gamedb:find_all(?feedback_collection, {game_name, GameName}) of
        {ok, []} ->
            <<"-">>;
        {ok, DbItemList} ->
            FeedbackList = [proplists:get_value(feedback, DbItem) || DbItem <- DbItemList],
            create_feedback(FeedbackList);
        _Error -> <<"-">>
end.

create_feedback(FeedbackList) -> create_feedback(FeedbackList, <<"">>).

create_feedback([], FeedbackCounter) -> FeedbackCounter;
create_feedback([Feedback | FeedbackList], FeedbackCounter) ->
    NewFeedbackCounter = case FeedbackCounter of
        <<"">> ->
            <<Feedback/binary, "<br/>">>;
        _ ->
            <<FeedbackCounter/binary, "-<br/>", Feedback/binary, "<br/>">>
    end,
    create_feedback(FeedbackList, NewFeedbackCounter).
