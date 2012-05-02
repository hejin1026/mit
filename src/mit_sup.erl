-module(mit_sup).

-created("hejin 2012-4-26").

-behavior(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
    Mit = {mit, {mit, start_link, []},
			permanent, 10, worker, [mit]},

    Mit = {mit_agent, {mit_agent, start_link, []},
			permanent, 10, worker, [mit_agent]},

	Dict = {mit_dict, {mit_dict, start_link, []},
		permanent, 10, worker, [mit_dict]},

	Mgr = {mit_mgr, {mit_mgr, start_link, []},
		permanent, 10, worker, [mit_mgr]},

    Olt = {mit_olt, {mit_olt, start_link, []},
		permanent, 10, worker, [mit_olt]},

    Onu = {mit_onu, {mit_onu, start_link, []},
		permanent, 10, worker, [mit_onu]},

    Board = {mit_board, {mit_board, start_link, []},
		permanent, 10, worker, [mit_board]},

    Port = {mit_port, {mit_port, start_link, []},
		permanent, 10, worker, [mit_port]},

    Fiber = {mit_fiber, {mit_fiber, start_link, []},
		permanent, 10, worker, [mit_fiber]},

    Gem = {mit_gem, {mit_gem, start_link, []},
		permanent, 10, worker, [mit_gem]},

    Vlan = {mit_vlan, {mit_vlan, start_link, []},
		permanent, 10, worker, [mit_vlan]},

    Event = {mit_event, {mit_event, start_link, []},
		permanent, 10, worker, [mit_event]},

	{ok, {{one_for_one, 10, 10}, [Mit, Dict, Mgr, Olt, Onu, Board, Port, Fiber, Gem, Vlan, Event]}}.
