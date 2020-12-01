-module(wave).

-include("logger.hrl").

-export([get_state/2, create_wave/3]).
-export([driving_wave/1, adjustment_wave/0, create_wave/1, span_wave/2]).

% -define(GOLD_LESS, [0.09, 0.146, 0.236, 0.382, 0.5, 0.618, 0.764, 0.854, 0.91]).
% -define(GOLD_MORE, [1.099, 1.171, 1.309, 1.618, 2, 2.618, 4.237, 6.849, 11.111]).
-define(GOLD_LESS, [0.382, 0.382, 0.5, 0.5, 0.5, 0.618, 0.618, 0.618, 0.764, 0.764, 0.764]).
-define(GOLD_MORE, [1, 1.309, 1.618, 2, 2.618]).
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
    [80000000, 90000000, 70000000, 80000000, 60000000, 70000000, 50000000, 60000000, 40000000, 50000000].

% five_wave() ->
%     Ar = 1,
%     Br = Ar*rd:take(?GOLD_LESS),    
%     Cr = Ar*rd:take(?GOLD_MORE),
%     Dr = Cr*rd:take(?GOLD_LESS),
%     Er = Dr*rd:take(?GOLD_MORE),
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
    check_wave(W).
    % L = lists:mapfoldl(fun(_X, N) ->
    %     {N, N+1}
    % end, 1, R),
    % {L, R}.

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
    Ratios = driving_wave(7),
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
    Coefficients = span_driving_coefficient(List, 1, []),
    % ?DEBUG("Coefficients:~p~n", [Coefficients]),
    span_ratio(Coefficients).

span_driving_coefficient([], _, Results) ->
    lists:reverse(Results);
span_driving_coefficient([H|T], Base, Results) ->
    Ratio =
    case H of
        1 ->
            1;
        N when N rem 2 == 1 ->
            -Base*rd:take(?GOLD_MORE);
        _ ->
            -Base*rd:take(?GOLD_LESS)
    end,
    span_driving_coefficient(T, Ratio, [Ratio|Results]).

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
            rd:take(?GOLD_ADJST_MORE);
        _ ->
            -rd:take(?GOLD_ADJST_LESS)
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
