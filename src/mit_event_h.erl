%%%----------------------------------------------------------------------
%%% File    : mit_event_h.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : MIT event handler
%%% Created : 28 Feb 2008
%%% License : http://www.opengoss.com/license
%%%
%%% Copyright (C) 2007-2008, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(mit_event_h).

-author('ery.lee@gmail.com').

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("elog/include/elog.hrl").

%TODO: refactor later.
init(_) ->
	{ok, state}.
	
handle_event({present, _Dn, _Entry}, State) ->
    handle_event({insert, _Dn, _Entry}, State),
	{ok, State};

handle_event({insert, _Dn, _Entry}, State) ->
    %TODO:?????
	{ok, State};

handle_event({update, Dn, Entry}, State) ->
    {value, OperState} = dataset:get_value(oper_state, Entry),
    case OperState == 2 of
    true ->
        ?INFO("delete_avail: ~p", [Dn]);
    false ->
        ok
    end,
	{ok, State};

handle_event({delete, _Dn}, State) ->
	{ok, State};

handle_event(Event, State) -> 
	?ERROR("Unexecpted event: ~p", [Event]),
	{ok, State}.

handle_call(Request, State) ->
	?ERROR("Unexpected request: ~p", [Request]),
	{ok, ok, State}.
	
handle_info(Info, State) ->
	?ERROR("Unexpected info: ~p", [Info]),
	{ok, State}.

terminate(_Arg, _State) ->
	ok.

code_change(_OldVsn, _State, _Extra) ->
	ok.
