%%%----------------------------------------------------------------------
%%% File    : mit_event.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : MIT change callback.
%%% Created : 28 Feb. 2008
%%% Updated : 07 Dec. 2009
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2007-2009, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(mit_event). 

-author('ery.lee@gmail.com').

-include_lib("elog.hrl").

-export([start_link/0, stop/0]).

-export([notify/1, add_event_handler/2, delete_event_handler/2]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_event:start_link({local, ?MODULE}).

stop() ->
	gen_event:stop(?MODULE).

add_event_handler(Handler, Args) ->
	gen_event:add_handler(?MODULE, Handler, Args).

delete_event_handler(Handler, Args) ->
	gen_event:delete_handler(?MODULE, Handler, Args).

notify(Event) ->
	gen_event:notify(?MODULE, Event).

