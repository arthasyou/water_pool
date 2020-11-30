-module(short_uuid).

-export([uuid/0]).

uuid() ->
    U = uuid:to_string(uuid:uuid1()),
    [A,_,_,B|_] = string:split(U, "-", all),
    A++B.