-module(mit_event).

-include_lib("elog/include/elog.hrl").

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
    io:format("start mit event ~n",[]),
    gen_event:start_link({local, ?MODULE}).

stop() ->
	gen_event:stop(?MODULE).

add_event_handler(Handler, Args) ->
    ?INFO("add handler :~p", [Handler]),
	gen_event:add_handler(?MODULE, Handler, Args).

delete_event_handler(Handler, Args) ->
    ?INFO("delete handler :~p", [Handler]),
	gen_event:delete_handler(?MODULE, Handler, Args).

notify(Event) ->
    ?INFO("notify : ~p", [Event]),
	gen_event:notify(?MODULE, Event).

