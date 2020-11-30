-module(pool_callback).

-include("logger.hrl").
-include("pool.hrl").

-define(SPEED_RATE, 1000). %% 万分比

%%%===================================================================
%%% API
%%%===================================================================

-export([draw/3]).
-export([ascent_run/2, fall_run/2, run/2]).
-export([pool_status/2]).

draw(Odds, Data, Flag) ->
    #pool_data{
        ratio = Ratio,
        segment = Segment,
        pot = Pot,
        pot_ratio = PotRatio,
        base_line = BaseLine,
        boundary = Boundary,
        wave = Wave,
        suction = Suction,
        bonus = Bonus
    } = Data,
    {NewPot, NewSuction} = increase_pot(Pot, PotRatio, Suction, Ratio),
    
    {Result, NewWave, NewSegment, ReNewPot, NewBonus} =
    case Flag of
        ascent ->
            ascent(Odds, NewPot, Segment, Ratio, Wave, BaseLine, Boundary, Bonus);
        fall ->
            fall(Odds, NewPot, Segment, Ratio, Wave, BaseLine, Boundary, Bonus)
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
        pot => Pot,
        wave => Data#pool_data.wave,
        segment => Data#pool_data.segment,
        suction => Suction,
        bonus => Bonus,
        brokerage => Suction * BrokerageRatio div Ratio
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
    

%%%===================================================================
%%% Internal
%%%===================================================================

ascent(Odds, Pot, Segment, Ratio, Wave, BaseLine, Boundary, Bonus) ->
    case analyzing_ascent(Odds, Pot, Segment) of
        true ->
            case ascent_run(Odds, Ratio) of
                true ->
                    {NewPot, NewBonus} = decrease_pot(Pot, Odds, Bonus),
                    {true, Wave, Segment, NewPot, NewBonus};
                false ->
                    ascent_action(Pot, Segment, Wave, BaseLine, Boundary, Bonus)
            end;
        false ->
            ascent_action(Pot, Segment, Wave, BaseLine, Boundary, Bonus)
    end.

fall(Odds, Pot, Segment, Ratio, Wave, BaseLine, Boundary, Bonus) ->
    case analyzing_fall(Odds, Pot, BaseLine, Segment) of
        true ->
            case fall_run(Odds, Ratio) of
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

ascent_run(Odds, Ratio) ->
    NewOdds = Odds + Odds*?SPEED_RATE div Ratio,
    run(NewOdds, Ratio).

fall_run(Odds, Ratio) ->
    NewOdds = Odds - Odds*?SPEED_RATE div Ratio,
    run(NewOdds, Ratio).

run(Odds, Ratio) ->
    case Odds < Ratio of
        true ->
            true;
        false ->
            Rand = rand:uniform(Odds),
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