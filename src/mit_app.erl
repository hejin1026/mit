%%%----------------------------------------------------------------------
%%% File    : mit_app.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : mit application
%%% Created : 07 Dec 2009
%%% License : http://www.opengoss.com/
%%%
%%% Copyright (C) 2007-2009, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(mit_app).

-author('ery.lee@gmail.com').

-behavior(application).

-export([start/2, stop/1]).

start(normal, _Args) ->
	{ok, SupPid} = mit_sup:start_link(),
	lists:foreach(
		fun({Name, Thunk}) when is_function(Thunk) -> 
			io:format("~n~s: ~s is starting...", [node(), Name]),
			Thunk(),
			io:format("[done]~n");
		   ({Name, Server}) when is_atom(Server) ->
			io:format("~n~s: ~s is starting...", [node(), Name]),
			start_child(SupPid, Server),
			io:format("[done]~n")
		end,
	 	[{"mit", mit},
		 {"mit dict", mit_dict},
		 {"mit mgr", mit_mgr},
		 {"mit olt", mit_olt},
		 {"mit onu", mit_onu},
		 {"mit board", mit_board},
		 {"mit port", mit_port},
		 {"mit fiber", mit_fiber},
		 {"mit gem", mit_gem},
		 {"mit vlan", mit_vlan},
		 {"mit event", fun() -> 
			start_child(SupPid, mit_event),
			mit_event:add_event_handler(mit_event_h, [])
			end}
		]),
	{ok, SupPid}.	

stop(_) ->
	ok.

%%Internal functions
start_child(SupPid, Name) ->
    {ok, _ChiId} = supervisor:start_child(SupPid, worker_spec(Name)).

worker_spec(Name) ->
    {Name, {Name, start_link, []}, 
        permanent, 10, worker, [Name]}.

