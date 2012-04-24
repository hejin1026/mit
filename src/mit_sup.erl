%%%----------------------------------------------------------------------
%%% File    : mit_sup.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : mit supervisor
%%% Created : 07 Dec 2009
%%% License : http://www.opengoss.com/
%%%
%%% Copyright (C) 2007-2009, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(mit_sup).

-author("ery.lee@gmail.com").

-behavior(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
	{ok, {{one_for_one, 10, 10}, []}}. 
