-module(water).

-include("logger.hrl").

-export([start/0, draw/2, create_pool/5]).

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
    {ok, #{hit => Flag}}.


%%%===================================================================
%%% Internal
%%%===================================================================

do_create_pool(FaceValue, Bullet, Brokerage, Ratio, Advance) ->
    Query = db:insert(pool, [
        {bullet, Bullet},
        {face_value, FaceValue},
        {brokerage, Brokerage},
        {ratio, Ratio},
        {advance, Advance},
        {pot, Advance}
    ]),
    % db:query(Query ++ "; SELECT LAST_INSERT_ID() as id;").
    case db:query(Query ++ "; SELECT LAST_INSERT_ID() as id;") of
        {ok, [#{id := ID}]} ->
            pool_sup:start_child([ID]),
            {ok, ID};
        _ ->
            {error, 1}
    end.