-module(mit_ctl).

-created("chibj 2012-9-25").

-export([start/0, process/1]).

-include_lib("mit/include/mit.hrl").

-define(STATUS_SUCCESS, 0).

-define(STATUS_ERROR,   1).

-define(STATUS_USAGE,   2).

-define(STATUS_BADRPC,  3).

start() ->
    case init:get_plain_arguments() of
        [SNode|Args] ->
            SNode1 = case string:tokens(SNode, "@") of
                [_Node, _Host] ->
                    SNode;
                _ ->
                    case net_kernel:longnames() of
                        true ->
                            SNode ++ "@" ++ inet_db:gethostname() ++ "." ++ inet_db:res_option(domain);
                       false ->
                            SNode ++ "@" ++ inet_db:gethostname()
                    end
            end,
            Node = list_to_atom(SNode1),
            Status = case rpc:call(Node, ?MODULE, process, [Args]) of
                 {badrpc, Reason} ->
                     io:format("RPC failed on the node ~p: ~p~n,~p", [Node, Args, Reason]),
                     ?STATUS_BADRPC;
                 S ->
                     S
                 end,
            halt(Status);
        _ ->
            io:format("you make a mistake, wash and go to bed", []),
            halt(?STATUS_USAGE)
        end.


%%get
process(["status"]) ->
    {InternalStatus, _} = init:get_status(),
    io:format("node ~p is ~p", [node(), InternalStatus]),
    case lists:keysearch(evabus, 1, application:which_applications()) of
	false ->
		io:format("mit is not running~n", []);
	{value,_Version} ->
		io:format("mit is running~n", [])
    end,
    ?STATUS_SUCCESS;

process(["status", "worker_pool"]) ->
    Status = worker_pool:status(),
    io:format("worker_pool status :~p~n", [Status]),
    ?STATUS_SUCCESS;

process(["stop"]) ->
	mit_app:stop(),
    init:stop(),
    ?STATUS_SUCCESS.


