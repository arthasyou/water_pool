-module(pool_dict).

-compile(export_all).

put_miss_count(Val) ->
    put(miss_count, Val).
get_miss_count() ->
    get(miss_count).

put_big_bonus(Val) ->
    put(big_bonus, Val).
get_big_bonus() ->
    get(big_bonus).

put_ascent_big_bonus_base_line(Val) ->
    put(ascent_big_bonus_base_line, Val).
get_ascent_big_bonus_base_line() ->
    get(ascent_big_bonus_base_line).