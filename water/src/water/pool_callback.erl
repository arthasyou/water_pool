-module(pool_callback).

-include("logger.hrl").
-include("pool.hrl").

-define(SPEED_RATE, 1000). %% 万分比
-define(ASCENT_SPEED_RATE, 7000). %% 万分比

%%%===================================================================
%%% API
%%%===================================================================

-export([draw/3]).
-export([pool_status/2, pool_data/1]).
-export([add_advance/2]).

draw(OddsRaw, Data, Flag) ->
    #pool_data{
        ratio = Ratio,
        segment = Segment,
        pot = Pot,
        pot_ratio = PotRatio,
        brokerage_ratio = BrokerageRatio,
        base_line = BaseLine,
        boundary = Boundary,
        wave = Wave,
        suction = Suction,
        bonus = Bonus
    } = Data,
    Odds = erlang:trunc(OddsRaw * Ratio), 
    {NewPot, NewSuction} = increase_pot(Pot, PotRatio, Suction, Ratio),
    
    {Result, NewWave, NewSegment, ReNewPot, NewBonus} =
    case Flag of
        ascent ->
            ascent(Odds, NewPot, Segment, Ratio, Wave, BaseLine, Boundary, Bonus);
        fall ->
            fall(Odds, NewPot, Segment, Ratio, Wave, BaseLine, Boundary, Bonus, BrokerageRatio)
    end,
    NewData = Data#pool_data{
        pot = ReNewPot,
        wave = NewWave,
        segment = NewSegment,
        suction = NewSuction,
        bonus = NewBonus
    },
    State = wave:get_state(ReNewPot, NewWave),
    {State, NewData, Result}.

%%%===================================================================

pool_status(State, Data) ->
    #pool_data{
        ratio = Ratio,
        pot = Pot,
        suction = Suction,
        bonus = Bonus,
        brokerage_ratio = BrokerageRatio
    } = Data,
    #{
        state => State,
        pot => Pot / Ratio,
        wave => Data#pool_data.wave,
        segment => Data#pool_data.segment,
        suction => Suction div Ratio,
        bonus => Bonus / Ratio,
        brokerage => Suction * BrokerageRatio div Ratio / Ratio,
        big_bonus => pool_dict:get_big_bonus(),
        bb_line => pool_dict:get_ascent_big_bonus_base_line(),
        miss_count => pool_dict:get_miss_count()
    }.
    % #{
    %     state => State,
    %     pot => Pot div Ratio * Bullet,
    %     wave => Data#pool_data.wave,
    %     segment => Data#pool_data.segment,
    %     suction => Suction div Ratio * Bullet,
    %     bonus => Bonus div Ratio * Bullet,
    %     brokerage => Suction * BrokerageRatio div (Ratio * Ratio) * Bullet
    % }.

pool_data(Data) ->
    #pool_data{
        ratio = Ratio,
        pot = Pot,
        suction = Suction,
        bonus = Bonus,
        brokerage_ratio = BrokerageRatio
    } = Data,
    {Head, _} = Data#pool_data.segment,
    Wave = lists:map(fun(X) ->
        X / Ratio
    end, [Head|Data#pool_data.wave]),
    #{
        pot => Pot / Ratio,
        wave => Wave,
        suction => Suction div Ratio,
        bonus => Bonus / Ratio,
        brokerage => Suction * BrokerageRatio div Ratio / Ratio
    }.

%%%===================================================================
add_advance(ValRaw, Data) ->
    #pool_data{
        ratio = Ratio,
        pot = Pot,
        advance = Advance,
        base_line = BaseLine
    } = Data,
    Val = ValRaw * Ratio,
    Boundary = Pot + Val,
    NewAdvance = Val+Advance,
    NewPot = Pot + Val,
    {Wave, Segment} = fresh_wave(NewPot, BaseLine, Boundary),
    NewData = Data#pool_data{
        pot = NewPot,
        wave = Wave,
        segment = Segment,
        advance = NewAdvance
    },
    State = wave:get_state(NewPot, Wave),
    {State, NewData}.

    

%%%===================================================================
%%% Internal
%%%===================================================================

ascent(Odds, Pot, Segment, Ratio, Wave, BaseLine, Boundary, Bonus) ->
    case analyzing_ascent(Odds, Pot, Segment) of
        true ->
            case ascent_run(Odds, Pot, Ratio) of
                true ->
                    {NewPot, NewBonus} = decrease_pot(Pot, Odds, Bonus),
                    {true, Wave, Segment, NewPot, NewBonus};
                false ->
                    ascent_action(Pot, Segment, Wave, BaseLine, Boundary, Bonus)
            end;
        false ->
            ascent_action(Pot, Segment, Wave, BaseLine, Boundary, Bonus)
    end.

fall(Odds, Pot, Segment, Ratio, Wave, BaseLine, Boundary, Bonus, BrokerageRatio) ->
    case analyzing_fall(Odds, Pot, BaseLine, Segment) of
        true ->
            case fall_run(Odds, Ratio, BrokerageRatio) of
                true ->
                    fall_action(Odds, Pot, Segment, Wave, BaseLine, Boundary, Bonus);
                false ->
                    {false, Wave, Segment, Pot, Bonus}
            end;
        win ->
            fall_action(Odds, Pot, Segment, Wave, BaseLine, Boundary, Bonus);
        false ->
            {NewWave, NewSegment} = fresh_wave(Pot, BaseLine, Boundary),
            {false, NewWave, NewSegment, Pot, Bonus}
    end.

%% ===================================================================

increase_pot(Pot, PotRatio, Suction, Ratio) ->
    NewPot = Pot + PotRatio,
    NewSuction = Suction + Ratio,
    {NewPot, NewSuction}.

decrease_pot(Pot, Odds, Bonus) ->
    NewPot = Pot - Odds,
    NewBonus = Bonus + Odds,
    {NewPot, NewBonus}.

%% ===================================================================

analyzing_ascent(Odds, Pot, Segment) ->
    {Bottom, _} = Segment,
    case Bottom >= Pot - Odds of
        true ->
            false;
        false ->
            true
    end.

analyzing_fall(Odds, Pot, BaseLine, Segment) ->
    case BaseLine >= Pot - Odds of
        true ->
            false;
        false ->
            {Top, _} = Segment,
            case Pot > Top of
                true ->
                    win;
                false ->
                    true
            end
    end.

%% ===================================================================

ascent_run(Odds, _Pot, Ratio) ->
    % NewOdds = Odds + Odds*?ASCENT_SPEED_RATE div Ratio,
    % run(NewOdds, Ratio).    
    % case big_bonus:ascent_draw(Odds, Pot, Ratio) of
    %     true ->
    %         true;
    %     false ->
    %         NewOdds = Odds + Odds*?SPEED_RATE div Ratio,
    %         run(NewOdds, Ratio)
    % end.

    case big_bonus:ascent_suction(Odds, Ratio) of
        true ->
            NewOdds = Odds + Odds*?SPEED_RATE div Ratio,
            run(NewOdds, Ratio);
        false ->
            false
    end.

fall_run(Odds, Ratio, BrokerageRatio) ->
    case big_bonus:fall_draw(Odds, Ratio, BrokerageRatio) of
        true ->
            true;
        false ->
            NewOdds = Odds + Odds*?SPEED_RATE div Ratio,
            Result = run(NewOdds, Ratio),
            case Result of
                false ->
                    big_bonus:increase_miss();
                _ ->
                    ok
            end,
            Result
    end.

run(Odds, Ratio) ->
    case Odds < Ratio of
        true ->
            true;
        false ->
            Rand = rand1:range(1,Odds),
            Rand =< Ratio
    end.

%% ===================================================================

ascent_segment(Pot, Segment, Wave, BaseLine, Boundary) ->
    {_, Destination} = Segment,
    case Pot >= Destination of
        true ->
            reside_wave_and_segment(Pot, Wave, BaseLine, Boundary);
        _ ->
            {Wave, Segment}
    end.

fall_segment(Pot, Segment, Wave, BaseLine, Boundary) ->
    {_, Destination} = Segment,
    case Pot =< Destination of
        true ->
            reside_wave_and_segment(Pot, Wave, BaseLine, Boundary);
        _ ->
            {Wave, Segment}
    end.

reside_wave_and_segment(Pot, Wave, BaseLine, Boundary) ->
    NewWave = reside_wave(Pot, Wave, BaseLine, Boundary),
    NewSegment = segment:create_segment(NewWave, Pot),    
    {NewWave, NewSegment}.

reside_wave(Pot, Wave, BaseLine, Boundary) ->
    [_|T] = Wave,
    case T of
        [] ->
            wave:create_wave(Pot, BaseLine, Boundary);
        _ ->
            T
    end.

fresh_wave(Pot, BaseLine, Boundary) ->
    Wave = wave:create_wave(Pot, BaseLine, Boundary),
    Segment = segment:create_segment(Wave, Pot),
    {Wave, Segment}.

%% =================================================================== 

ascent_action(Pot, Segment, Wave, BaseLine, Boundary, Bonus) ->
    {NewWave, NewSegment} = ascent_segment(Pot, Segment, Wave, BaseLine, Boundary),
    {false, NewWave, NewSegment, Pot, Bonus}.

fall_action(Odds, Pot, Segment, Wave, BaseLine, Boundary, Bonus) ->
    {NewPot, NewBonus} = decrease_pot(Pot, Odds, Bonus),
    {NewWave, NewSegment} = fall_segment(NewPot, Segment, Wave, BaseLine, Boundary),
    {true, NewWave, NewSegment, NewPot, NewBonus}.