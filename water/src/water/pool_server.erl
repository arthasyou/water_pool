%%%-------------------------------------------------------------------
%%% @author luobin
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Nov 2020 3:31 PM
%%%-------------------------------------------------------------------
-module(pool_server).
-author("luobin").
-include("pool.hrl").
-include("logger.hrl").

-behaviour(gen_statem).

%% API
-export([start_link/1, draw/2, detail/1, pool_data/1, add_advance/2]).

%% gen_statem callbacks
-export([init/1, format_status/2, handle_event/4, terminate/3,
    code_change/4, callback_mode/0]).

%% state_name
% -export([ascent/3, fall/3]). 

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ID) ->
    gen_statem:start_link(?MODULE, [ID], []).

draw(ID, Odds) ->
    case pool_mgr:get_pid(ID) of
        {ok, PID} ->
            gen_statem:call(PID, {draw, Odds});
        _ ->
            fail
    end.

detail(ID) ->
    case pool_mgr:get_pid(ID) of
        {ok, PID} ->
            gen_statem:call(PID, detail);
        _ ->
            fail
    end.

pool_data(ID) ->
    case pool_mgr:get_pid(ID) of
        {ok, PID} ->
            gen_statem:call(PID, data);
        _ ->
            fail
    end.

add_advance(ID, Val) ->
    case pool_mgr:get_pid(ID) of
        {ok, PID} ->
            gen_statem:call(PID, {add_advance, Val});
        _ ->
            fail
    end.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init([ID]) ->
    process_flag(trap_exit, true),
    pool_mgr:register(ID, self()),
    {State, Data} = pool_init:init(ID),
    {ok, State, Data}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
    % state_functions.
    handle_event_function.

%% @private
%% @doc Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
format_status(_Opt, [_PDict, _State, _Data]) ->
    Status = some_term,
    Status.

%% @private
%% @doc There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.

% ascent({call, From}, {draw, Odds}, Data) ->
%     {NextState, NewData, Reply} = pool_callback:draw(Odds, Data, ascent),
%     {next_state, NextState, NewData, [{reply, From, Reply}]};

% ascent(_EventType, _EventContent, Data) ->
%     {keep_state, Data}.

% fall({call, From}, {draw, Odds}, Data) ->
%     {NextState, NewData, Reply} = pool_callback:draw(Odds, Data, fall),
%     {next_state, NextState, NewData, [{reply, From, Reply}]};
% fall(_EventType, _EventContent, Data) ->
%     {keep_state, Data}.

%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.

handle_event({call, From}, {draw, Odds}, State, Data) ->
    {NextState, NewData, Reply} = pool_callback:draw(Odds, Data, State),
    {next_state, NextState, NewData, [{reply, From, Reply}]};

handle_event({call, From}, detail, State, Data) ->
    Reply = pool_callback:pool_status(State, Data),
    {keep_state_and_data, [{reply, From, Reply}]};
handle_event({call, From}, data, _State, Data) ->
    Reply = pool_callback:pool_data(Data),
    {keep_state_and_data, [{reply, From, Reply}]};
handle_event({call, From}, {add_advance, Val}, _State, Data) ->
    {NextState, NewData} = pool_callback:add_advance(Val, Data),
    {next_state, NextState, NewData, [{reply, From, ok}]};

handle_event(info, sync_db, _State, Data) ->
    pool_init:sync_db(Data),
    keep_state_and_data;

handle_event(_EventType, _EventContent, _State, _Data) ->
    keep_state_and_data.

terminate(_Reason, _State, Data) ->
    % io:format("pid: ~p down~n", [self()]),
    pool_init:sync_db(Data),
    #pool_data{id = ID} = Data,
    pool_mgr:unregister(ID),
    
    ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
