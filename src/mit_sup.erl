-module(mit_sup).

-created("hejin 2012-4-26").

-behavior(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
    Workers = case mnesia:system_info(extra_db_nodes) of
    [] -> %master node
        Event = {mit_event, {mit_event, start_link, []},permanent, 10, worker, [mit_event]},
        Agent = {mit_agent, {mit_agent, start_link, []},permanent, 10, worker, [mit_agent]},
    	Mgr =   {mit_mgr, {mit_mgr, start_link, []},permanent, 10, worker, [mit_mgr]},
        [ Event, Agent, Mgr|worker()];
    _ -> %slave node
        worker()
    end,
	{ok, {{one_for_one, 10, 10}, Workers}}.

worker() ->
     Mit = {mit, {mit, start_link, []}, permanent, 10, worker, [mit]},

	Dict = {mit_dict, {mit_dict, start_link, []},permanent, 10, worker, [mit_dict]},

    Olt = {mit_olt, {mit_olt, start_link, []},permanent, 10, worker, [mit_olt]},

    Onu = {mit_onu, {mit_onu, start_link, []},permanent, 10, worker, [mit_onu]},

    Board = {mit_board, {mit_board, start_link, []},permanent, 10, worker, [mit_board]},

    Port = {mit_port, {mit_port, start_link, []},permanent, 10, worker, [mit_port]},

	Splite = {mit_splite, {mit_splite, start_link, []},permanent, 10, worker, [mit_splite]},

	Eoc = {mit_eoc, {mit_eoc, start_link, []},permanent, 10, worker, [mit_eoc]},

	Cpe = {mit_cpe, {mit_cpe, start_link, []},permanent, 10, worker, [mit_cpe]},

	Area = {mit_area, {mit_area, start_link, []},permanent, 10, worker, [mit_area]},

    Fiber = {mit_fiber, {mit_fiber, start_link, []},permanent, 10, worker, [mit_fiber]},

    [Mit, Dict, Olt, Onu,Eoc,Cpe,Area,Board,Port,Splite, Fiber].
