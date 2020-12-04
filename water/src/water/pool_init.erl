-module(pool_init).

-include("logger.hrl").
-include("pool.hrl").

-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================
init(ID) ->
    Qeury = db:select('pool') ++ db:where([{id, "=", ID}]),
    {ok, [Item]} = db:query(Qeury),
    Data = get_data(ID, Item),
    NewData= reside_wave(Data),
    State = get_state(NewData),
    {State, NewData}.

%%%===================================================================
%%% Internal
%%%===================================================================
get_data(ID, Item) ->
    #{        
        advance := Advance,
        base_line := BaseLine,
        bonus := Bonus,
        boundary := Boundary,
        brokerage := BrokerageRatio,
        bullet := Bullet,
        pot := Pot,
        jackpot := JackPot,
        ratio := Ratio,
        face_value := FaceValue,
        suction := Suction,
        wave := Wave,
        segment := Segment
    } = Item,
    #pool_data{
        id = ID,
        ratio = Ratio,
        face_value = FaceValue,
        brokerage_ratio = BrokerageRatio, 
        pot_ratio = Ratio - BrokerageRatio,       
        bullet = Bullet,         
        pot = Pot*Ratio,
        base_line = BaseLine,      
        boundary = Boundary*Ratio,       
        suction = Suction,        
        bonus = Bonus,         
        jackpot = JackPot,        
        advance = Advance,
        wave = Wave,
        segment = Segment
    }.

get_state(Data) ->
    #pool_data{
        wave = Wave,
        pot = Pot
    } = Data,
    wave:get_state(Pot, Wave).

reside_wave(Data) ->
    #pool_data{
        wave = Wave,
        segment = Segment,
        pot = Pot,
        base_line = BaseLine,
        boundary = Boundary
    } = Data,
    NewWave =
    case Wave of
        undefined ->
            wave:create_wave(Pot, BaseLine, Boundary);
        [] ->
            wave:create_wave(Pot, BaseLine, Boundary);
        _ ->
            Wave
    end,  
    NewSegment = reside_segment(NewWave, Segment, Pot),
    Data#pool_data{
        wave = NewWave,
        segment = NewSegment
    }.

reside_segment(Wave, Segment, Pot) ->
    case Segment of
        undefined ->
            segment:create_segment(Wave, Pot);
        _ ->
            Segment
    end.