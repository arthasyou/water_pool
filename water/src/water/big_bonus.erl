-module(big_bonus).

-export([init_miss_count/0, init_ascent_base_line/1, increase_miss/0]).
-export([fall_draw/3, ascent_draw/3, ascent_suction/2]).

-define(BIG_BONUS, [160, 180, 240]).
% -define(BIG_BONUS, [100]).

-define(BIG_BONUS_BASE_LINE_SPEED, 100).
-define(SUCTION_RATIO, 20).

%%%===================================================================
%%% API
%%%===================================================================
init_miss_count() ->
    pool_dict:put_miss_count(0).

init_ascent_base_line(Point) ->
    pool_dict:put_ascent_big_bonus_base_line(Point).

increase_miss() ->
    V = pool_dict:get_miss_count(),
    pool_dict:put_miss_count(V+1).

fall_draw(Odds, Ratio, BrokerageRatio) ->
    case check_big_bonus(Odds, Ratio) of
        true ->
            do_fall_draw(Odds, Ratio, BrokerageRatio);
        false ->
            false
    end.

ascent_draw(Odds, Pot, Ratio) ->
    case check_big_bonus(Odds, Ratio) of
        true ->
            do_ascent_draw(Odds, Pot, Ratio);
        false ->
            false
    end.

ascent_suction(Odds, Ratio) ->
    case Odds > ?SUCTION_RATIO * Ratio of
        true ->
            false;
        false ->
            true
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

check_big_bonus(Odds, Ratio) ->
    % ?DEBUG("going here: ~p~n", [1111111]),
    BB = get_big_bonus(Ratio),
    case Odds == BB of
        true ->
            true;
        false ->
            false
    end.

get_big_bonus(Ratio) ->
    case pool_dict:get_big_bonus() of
        undefined ->
            span_big_bonus(Ratio);
        V ->
            V
    end.

span_big_bonus(Ratio) ->
    init_miss_count(),
    Item = rand1:range(?BIG_BONUS)*Ratio,
    pool_dict:put_big_bonus(Item),    
    Item.

%%%===================================================================

do_fall_draw(Odds, Ratio, BrokerageRatio) ->
    Count = pool_dict:get_miss_count(),
    Result = Count > (Odds / Ratio) * (1+BrokerageRatio/Ratio),
    case Result of
        true ->
            span_big_bonus(Ratio);
        _ ->
            ok
    end,
    Result.

do_ascent_draw(Odds, Pot, Ratio) ->
    Line = pool_dict:get_ascent_big_bonus_base_line(),
    Result = Pot - Odds > Line,
    case Result of
        true ->
            span_big_bonus(Ratio),
            init_ascent_base_line(Line+?BIG_BONUS_BASE_LINE_SPEED*Ratio);
        _ ->
            ok
    end,
    Result.