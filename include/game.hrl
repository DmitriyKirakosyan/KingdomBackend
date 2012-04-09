-record (flower, {id, time_need, profit_money}).
-record (user_flower, {id, time = 0, completed = false}).

-define (LEVEL_DOWN_TIMEOUT , 10000).
-record (user_state, {level = 1, leveldown_time = ?LEVEL_DOWN_TIMEOUT, money = 1000, food = 0, flower=#user_flower{id=flower1, time=0}, last_update}).

-record (town_object, {id, x, y}).

-define (town_price, 500).

-define (flower1, #flower{id=flower1, time_need=5000, profit_money=400}).
-define (flower2, #flower{id=flower2, time_need=7000, profit_money=500}).
-define(MAX_LEVEL, 20).
