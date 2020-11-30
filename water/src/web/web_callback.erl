%%%-------------------------------------------------------------------
%%% @author ysx
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Oct 2019 17:26
%%%-------------------------------------------------------------------
-module(web_callback).
-author("ysx").

%% ==================================================
%% API
%% ==================================================
-export([create_pool/1, draw/1]).

create_pool(DataIn) ->
    #{
        <<"face_value">> := FaceValue,
        <<"bullet">> := Bullet,
        <<"brokerage">> := Brokerage,
        <<"ratio">> := Ratio,
        <<"advance">> := Advance
    } = DataIn,    
    water:create_pool(FaceValue, Bullet, Brokerage, Ratio, Advance).

draw(DataIn) ->
    #{
        <<"id">> := ID,
        <<"odds">> := Odds
    } = DataIn,
    water:draw(ID, Odds).

%% ==================================================
%% Internal
%% ==================================================
