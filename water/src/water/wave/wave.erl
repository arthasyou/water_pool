-module(wave).

-include("logger.hrl").

-export([get_state/2, create_wave/3]).
-export([driving_wave/1, adjustment_wave/0, create_wave/1, span_wave/2]).

-define(GOLD_LESS, [0.4, 0.5, 0.6, 0.7]).
-define(GOLD_MORE, [1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8]).
-define(GOLD_FIVE, [0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3]).
-define(GOLD_ADJST_MORE, [1, 1.1, 1.2, 1.3, 1.4, 1.5]).
-define(GOLD_ADJST_LESS, [0.6, 0.7, 0.8, 0.9]).

%%%===================================================================
%%% API
%%%===================================================================
get_state(Pot, Wave) ->
    [H|_] = Wave,
    case Pot > H of
        true ->
            fall;
        false ->
            ascent
    end.

create_wave(Pot, BaseLine, Boundary) ->
    Down = Pot - BaseLine,
    Up = Boundary - Pot,
    Rand = rand:uniform(Down+Up),
    case Rand < Up of
        true ->
            span_wave(Pot, Boundary);
        false ->
            span_wave(Pot, BaseLine)
    end.
    % spec_wave().

%%%===================================================================
%%% Internal
%%%===================================================================

spec_wave() ->
    [90000000, 100000000].

%%%===================================================================

span_wave(From, To) ->
    Len = To - From,
    Wave = create_wave(Len),
    {W,_} = lists:mapfoldl(fun(X, Acc) ->
        Point = trunc(Acc + X),
        {Point, Point}
    end, From, Wave),
    R = check_wave(W),
    R.

check_wave(W) ->
    lists:map(fun(X) ->
        case X < 0 of
            true ->
                0;
            false ->
                X
        end
    end, W).

create_wave(Len) ->
    Ratios = driving_wave(5),
    Lens = ratio_to_len(Len, Ratios),
    create_sub_wave(Lens, 1, []).

create_sub_wave([], _N, Rs) ->
    Rs;
create_sub_wave([H|T], N, Rs) ->
    Ratios =
    case N rem 2 of
        1 ->
            driving_wave(5);
        0 ->
            adjustment_wave()
    end,
    Wave = ratio_to_len(H, Ratios),
    NewRs = Rs ++ Wave,
    create_sub_wave(T, N+1, NewRs).



ratio_to_len(Len, Ratios) ->
    {List, _} =
    lists:mapfoldl(fun(X, Outset) ->
        Destination = Len * X,
        NewLen = Destination - Outset,
        {NewLen, Destination}
    end, 0, Ratios),
    List.

%%%===================================================================

driving_wave(N) ->
    List = lists:seq(1, N),
    Coefficients = span_driving_coefficient(List, 1, 1, []),
    span_ratio(Coefficients).

span_driving_coefficient([], _, _, Results) ->
    lists:reverse(Results);
span_driving_coefficient([_], Base, LastWave, Results) ->
    Ratio = -LastWave*rand1:range(?GOLD_FIVE),
    span_driving_coefficient([], Base, Ratio, [Ratio|Results]);
span_driving_coefficient([H|T], Base, LastWave, Results) ->
    Ratio =
    case H of
        1 ->
            1;            
        N when N rem 2 == 1 ->
            Base*rand1:range(?GOLD_MORE);
        _ ->
            -LastWave*rand1:range(?GOLD_LESS)
    end,
    span_driving_coefficient(T, Base, Ratio, [Ratio|Results]).

adjustment_wave() ->
    List = lists:seq(1, 3),
    Coefficients = span_adjustment_coefficient(List, []),
    span_ratio(Coefficients).

span_adjustment_coefficient([], Results) ->
    lists:reverse(Results);
span_adjustment_coefficient([H|T], Results) ->
    Ratio =
    case H of
        1 ->
            1;
        N when N rem 2 == 1 ->
            rand1:range(?GOLD_ADJST_MORE);
        _ ->
            -rand1:range(?GOLD_ADJST_LESS)
    end,
    span_adjustment_coefficient(T, [Ratio|Results]).

span_ratio(List) ->
    Base = 1/lists:sum(List),
    span_ratio(List, Base, 0, []).

span_ratio([], _, _, Results) ->
    lists:reverse(Results);
span_ratio([H|T], Base, Last, Results) ->
    Ratio = Base*H+Last,
    span_ratio(T, Base, Ratio, [Ratio|Results]).


%%%===================================================================
