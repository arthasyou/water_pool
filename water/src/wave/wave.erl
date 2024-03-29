-module(wave).

-include("logger.hrl").

-export([get_state/2, create_wave/3]).
-export([driving_wave/1, adjustment_wave/0, create_wave/1, span_wave/2]).

% -define(GOLD_LESS, [0.09, 0.146, 0.236, 0.382, 0.5, 0.618, 0.764, 0.854, 0.91]).
% -define(GOLD_MORE, [1.099, 1.171, 1.309, 1.618, 2, 2.618, 4.237, 6.849, 11.111]).
-define(GOLD_LESS, [0.382, 0.382, 0.5, 0.5, 0.5, 0.618, 0.618, 0.618, 0.764, 0.764, 0.764]).
-define(GOLD_MORE, [1, 1.309, 1.618]).
% -define(GOLD_MORE, [1, 1.309, 1.618, 2, 2.618]).
-define(GOLD_FIVE, [0.764, 1, 1.309, 1.618]).
-define(GOLD_ADJST_MORE, [1, 1.309, 1.618]).
-define(GOLD_ADJST_LESS, [0.618, 0.618, 0.764, 0.764, 0.764, 1, 1, 1, 1.171]).
-define(DRIVING_WAVES_QUANTITY, [5,7,9,11]).
-define(ADJUSTMENT_WAVES_QUANTITY, 3).

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
    % [80000000, 90000000, 70000000, 80000000, 60000000, 70000000, 50000000, 60000000, 40000000, 50000000].

% five_wave() ->
%     Ar = 1,
%     Br = Ar*rand1:range(?GOLD_LESS),
%     Cr = Ar*rand1:range(?GOLD_MORE),
%     Dr = Cr*rand1:range(?GOLD_LESS),
%     Er = Dr*rand1:range(?GOLD_MORE),
%     Sum = Ar-Br+Cr-Dr+Er,
%     A = 1/Sum,
%     B = A*Br,
%     C = A*Cr,
%     D = A*Dr,
%     E = A*Er,
%     Bp = A-B,
%     Cp = Bp+C,
%     Dp = Cp-D,
%     Ep = Dp+E,
%     [0, A, Bp, Cp, Dp, Ep].

%%%===================================================================

span_wave(From, To) ->
    Len = To - From,
    Wave = create_wave(Len),
    {W,_} = lists:mapfoldl(fun(X, Acc) ->
        Point = trunc(Acc + X),
        {Point, Point}
    end, From, Wave),
    W.

create_wave(Len) ->
    Ratios = driving_wave(5),
    Lens = ratio_to_len(Len, Ratios),
    create_level_wave(Lens, 3).

create_level_wave(Lens, 0) ->
    Lens;
create_level_wave(Lens, N) ->
    Lens2 = create_sub_wave(Lens, []),
    create_level_wave(Lens2, N-1).

create_sub_wave([], Rs) ->
    Rs;
create_sub_wave([H|T], Rs) ->
    {Len, Flag} = H,
    Ratios =
    case Flag of
        driving ->
            driving_wave(5);
        adjustment ->
            adjustment_wave()
    end,
    Wave = ratio_to_len(H, Ratios),
    NewRs = Rs ++ Wave,
    create_sub_wave(T, NewRs).



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
    Ratios = span_ratio(Coefficients),
    add_flag(Ratios).

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
    Ratios = span_ratio(Coefficients),
    add_flag(Ratios).

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

add_flag(Ratios) ->
    add_flag(Ratios, 1, []).
add_flag([], _, R) ->
    lists:reverse(R);
add_flag([H|T], N, R) ->
    Flag =
    case N rem 2 of
        1 ->
            driving;
        0 ->
            adjustment
    end,
    add_flag(T, N+1, [{H, Flag} | R]).


%%%===================================================================
