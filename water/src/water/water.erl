-module(water).

-include("logger.hrl").

-export([start/0, draw/2, create_pool/5, add_advance/2, pool_data/1]).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    Query = db:select([id], pool),
    {ok, Data} = db:query(Query),
    lists:foreach(fun(X) ->
        #{id := ID} = X,
        pool_sup:start_child([ID])
    end, Data),
    ok.

create_pool(FaceValue, Bullet, Brokerage, Ratio, Advance) ->
    case Ratio div 10 >= Brokerage of
        true ->
            case do_create_pool(FaceValue, Bullet, Brokerage, Ratio, Advance) of
                {ok, ID} ->
                    {ok, #{id => ID}};
                Error ->
                    Error
            end;
        false ->
            {error, 1001}
    end.    

draw(ID, Odds) -> 
    Flag = pool_server:draw(ID, Odds),
    case Flag of
        fail ->
            {error, 1002};
        _ ->
            {ok, #{hit => Flag}}
    end.

add_advance(ID, Val) ->
    Flag = pool_server:add_advance(ID, Val),
    case Flag of
        fail ->
            {error, 1};
        _ ->
            {ok, []}
    end.

pool_data(ID) ->
    Data = pool_server:pool_data(ID),
    case Data of
        fail ->
            {error, 1};
        _ ->
            {ok, Data}
    end.


%%%===================================================================
%%% Internal
%%%===================================================================

do_create_pool(FaceValue, Bullet, BrokerageRatio, Ratio, Advance) ->
    Pot = Advance*Ratio,
    Query = db:insert(pool, [
        {bullet, Bullet},
        {face_value, FaceValue},
        {brokerage, BrokerageRatio},
        {ratio, Ratio},
        {advance, Advance},
        {pot, Pot},
        {boundary, Pot}
    ]),
    case db:query(Query ++ "; SELECT LAST_INSERT_ID() as id;") of
        {ok, [#{id := ID}]} ->
            pool_sup:start_child([ID]),
            {ok, ID};
        _ ->
            {error, 1}
    end.