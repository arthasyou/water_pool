-module(web_reply).

%% ==================================================
%% API
%% ==================================================
-export([handle/2]).

handle(Path, DataIn) ->
    Reply = web_routes:routing(Path, DataIn),
    encode(Reply).


%% ==================================================
%% Internal
%% ==================================================
encode(Reply) ->    
    case Reply of
        {ok, Data} ->
            #{code => 0, data => Data};
        {error, Code} ->
            #{code => Code, reason => web_error_message:get_msg(Code)}
    end.