-module(rd).

-export([take/1]).

take([]) -> null;
take(List) -> 
    Len = length(List),
    Index = rand:uniform(Len),
    take_and_rest(List, Index, []).
%%
take_and_rest(List, 1, _Rest) ->
    [Term | _] = List,
    Term;
take_and_rest(List, Index, Rest) ->
    [H | R] = List,
    Nrest = [H | Rest],
    take_and_rest(R, Index - 1, Nrest).