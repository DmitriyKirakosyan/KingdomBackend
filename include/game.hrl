-record (flower, {id, time_need, profit_money}).
-record (user_flower, {id, time}).

-record (user_state, {money = 0, flowers = [], last_update}).

-define (town_price, 500).

-define (flower1, #flower{id=flower1, time_need=5, profit_money=400}).
-define (flower2, #flower{id=flower2, time_need=7, profit_money=500}).