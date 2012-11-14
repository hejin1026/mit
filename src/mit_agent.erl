-module(mit_agent).

-created("hejin 2012-4-26").

-include_lib("elog/include/elog.hrl").

-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {channel}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, Conn} = amqp:connect(),
    Channel = open(Conn),
    io:format("finish start mit agent...~n",[]),
    {ok, #state{channel = Channel}}.

open(Conn) ->
    {ok, Channel} = amqp:open_channel(Conn),
    amqp:queue(Channel, <<"ponoss.mit">>),
    amqp:consume(Channel, <<"ponoss.mit">>),
    Channel.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({deliver, <<"ponoss.mit">>, _Properties, Payload}, State) ->
    handle_datalist(binary_to_term(Payload)),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

handle_datalist(DataList) when is_list(DataList) ->
    ?INFO("get disco data :~p",[DataList]),
	lists:foreach(fun(Data) -> ?INFO("get disco data :~p",[Data]),
    handle_data(Data) end, DataList);
handle_datalist(Data)  ->
    handle_data(Data).


handle_data({entry, Type, Dn, []}) ->
	 ?WARNING("get disco data is [] dn:~p,Type:~p",[Dn,Type]);
handle_data({entry, olt, Dn, Attrs}) ->
    do(fun()->
        try mit_olt:add(Dn, Attrs)
        catch _:Err ->?ERROR("bad error: ~p,~p,~p", [Err,Dn, Attrs])
        end
       end);
   handle_data({entry, dslam, Dn, Attrs}) ->
       do(fun()->
           try mit_dslam:add(Dn, Attrs)
           catch _:Err ->?ERROR("bad error: ~p,~p,~p", [Err,Dn, Attrs])
           end
          end);
handle_data({entry, onu, Dn, Attrs})->
    do(fun()->
        try mit_onu:add(Dn, Attrs)
        catch _:Err ->?ERROR("bad error: ~p,~p,~p", [Err,Dn, Attrs])
        end
       end);
handle_data({entry, onus, OltDn, Attrs}) ->
	Fun = fun() ->
			try mit_onu:add_onus(OltDn, Attrs)
            catch
				_:Err ->?ERROR("bad error: ~p,~p,~p", [Err,Attrs,OltDn])
			end
		  end,
	do(Fun);
handle_data({entry, board, Dn, Attrs}) ->
    do(fun()->
        try mit_board:add(Dn, Attrs)
        catch _:Err ->?ERROR("bad error: ~p,~p,~p", [Err,Dn, Attrs])
        end
       end);
handle_data({entry, boards, OltDn, Attrs}) ->
	Fun = fun() ->
			try mit_board:add_boards(OltDn, Attrs)
            catch
				_:Err ->?ERROR("bad error: ~p,~p", [Err,Attrs,OltDn])
			end
		  end,
	do(Fun);
handle_data({entry, port, Dn, Attrs}) ->
    do(fun()->
        try mit_port:add(Dn, Attrs)
        catch _:Err ->?ERROR("bad error: ~p,~p,~p", [Err,Dn, Attrs])
        end
       end);
handle_data({entry, ports, Dn, Attrs}) ->%批量add 用户口 一个onu一次，dn为onu
	Fun = fun() ->
			try mit_port:add_ports(Dn, Attrs)  catch
				_:Err ->?ERROR("bad error: ~p,~p,~p", [Err,Attrs,Dn])
			end
		  end,
	do(Fun);
handle_data({entry, clt, Dn, Attrs}) ->
	mit_clt:add(Dn, Attrs);
handle_data({entry, cnu, Dn, Attrs}) ->
	mit_cnu:add(Dn, Attrs);
handle_data({entry, gem, Dn, Attrs}) ->
    do(fun()->
        try mit_gem:add(Dn, Attrs)
        catch _:Err ->?ERROR("bad error: ~p,~p,~p", [Err,Dn, Attrs])
        end
       end);
handle_data({entry, vlan, Dn, Attrs}) ->
    do(fun()->
        try mit_vlan:add(Dn, Attrs)
        catch _:Err ->?ERROR("bad error: ~p,~p,~p", [Err,Dn, Attrs])
        end
       end);
handle_data({operate, Operate}) ->
	?ERROR("operate_data normal :~p, ~n", [Operate]),
	handle_operate(Operate);
handle_data({hostinfo, HostInfo}) ->
    handle_hostinfo(HostInfo);
handle_data(_) ->
   ok.

do(Fun) ->
    worker_pool:submit_async(Fun).


handle_operate(Operate)	->
    DateTime = {datetime, {date(), time()}},
    {value, Id} = dataset:get_value(id, Operate),
    Operate1 = lists:keydelete(id, 1, Operate),
	case emysql:update(mit_operates, [{updated_at, DateTime} | Operate1], {id, Id}) of
        {error, Reason} ->
            ?ERROR("update Operate error :~p, ~n Reason: ~p", [Operate, Reason]);
        _ ->
            ok
     end.

handle_hostinfo(HostInfo) ->
    DateTime = {datetime, {date(), time()}},
    {value, JID} = dataset:get_value(jid, HostInfo),
    case emysql:select({servers, {jid, JID}}) of
        {ok, [_Record|_]} ->
            case emysql:update(servers, [{updated_at, DateTime} | HostInfo], {jid, JID}) of
                {error, Reason} ->
                    ?ERROR("insert host  :~p, ~n Reason: ~p", [HostInfo, Reason]);
                _ ->
                    ok
             end;
        {ok, []} ->
            case emysql:insert(servers, [{created_at, DateTime} | HostInfo]) of
                {error, Reason} ->
                    ?ERROR("insert host  :~p, ~n Reason: ~p", [HostInfo, Reason]);
                _ ->
                    ok
            end;
        {error, Reason} ->
            ?ERROR("~p",[Reason])
    end.
