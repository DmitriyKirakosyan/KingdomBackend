-record (flower, {id, time_need, profit_money}).
-record (user_flower, {id, time}).

-record (user_state, {money = 1000, flowers = [], last_update}).

-record (town_object, {id, x, y}).

-define (town_price, 500).

-define (flower1, #flower{id=flower1, time_need=5000, profit_money=400}).
-define (flower2, #flower{id=flower2, time_need=7000, profit_money=500}).