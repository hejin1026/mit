-module(mit_sup).

-created("hejin 2012-4-26").

-behavior(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
    Workers = case application:get_env(mode) of
    {ok, master} -> %master node
        Event = {mit_event, {mit_event, start_link, []},permanent, 10, worker, [mit_event]},
        Agent = {mit_agent, {mit_agent, start_link, []},permanent, 10, worker, [mit_agent]},
    	Mgr =   {mit_mgr, {mit_mgr, start_link, []},permanent, 10, worker, [mit_mgr]},
        Dict = {mit_dict, {mit_dict, start_link, []},permanent, 10, worker, [mit_dict]},
        [ Event, Agent, Mgr,Dict|worker()];
    _ -> %slave node
        worker()
    end,
	{ok, {{one_for_one, 10, 10}, Workers}}.

worker() ->
     Mit = {mit, {mit, start_link, []}, permanent, 10, worker, [mit]},
     Area = {mit_area, {mit_area, start_link, []},permanent, 10, worker, [mit_area]},
    [Mit,Area].
