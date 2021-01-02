-module(segment).

%%%===================================================================
%%% API
%%%===================================================================
-export([create_segment/2]).

create_segment(Wave, Pot) ->
    [H|_] = Wave,
    case Pot < H of
        true ->
            big_bonus:init_ascent_base_line(Pot);
        false ->
            big_bonus:init_miss_count()
    end,
    self() ! sync_db,
    {Pot,H}.



%%%===================================================================
%%% Internal
%%%===================================================================
