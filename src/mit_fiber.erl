
-module(mit_fiber).

-author('hejin1026@gmail.com').

-include("mit.hrl").
-include_lib("elog/include/elog.hrl").

-behavior(gen_server).

-export([start_link/0,
         stop/0]).

%api
-export([attrs/0,
         lookup/1,
		 add/2,
		 update/2]).

-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3]).

-import(extbif, [to_list/1, to_binary/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, stop).

attrs() ->
    [
        oper_state, %0断，1不断
        device_id,
        device_type
    ].


lookup(Dn) ->
    case mit:lookup(Dn) of
    {ok, #entry{data = Fiber, type = fiber}} ->
        {ok, Fiber};
    false ->
        false;
    _Other ->
        false
    end.

add(Dn, Fiber) ->
	gen_server:cast(?MODULE, {add, Dn, Fiber}).

update(Dn, Attrs) ->
    gen_server:cast(?MODULE, {update, Dn, Attrs}).

init([]) ->
    case mnesia:system_info(extra_db_nodes) of
        [] -> %master node
            do_init();
        _ -> %slave node
            ok
    end,
    {ok, state}.


do_init() ->
    case emysql:sqlquery("select * from fault_events where
        (alarm_name = 'hwolteponlumlost' or alarm_name = 'hwAlarmonusingledown') and alarm_state <> 3  ") of
    {ok, Faults} ->
        lists:foreach(fun(Fault) ->
            {value, PortId} = dataset:get_value(device_id, Fault),   %port的
            {value, PortType} = dataset:get_value(device_type, Fault),
            {value, AlarmSource} = dataset:get_value(alarm_source, Fault),
            case mit:lookup(to_binary(AlarmSource)) of
            {ok, _Entry} ->
                Uid = get_uid(PortType, PortId),
                Dn = "fiber," ++ to_list(AlarmSource),
                Data = [{oper_state, 0}, {device_id, PortId}, {device_type, PortType}],
                mit:update(#entry{dn = to_binary(Dn), uid = to_binary(Uid), type = fiber, parent = mit_util:bdn(Dn), data = Data});
            false ->
                io:format("cannot find device: ~p ~n,~p", [PortId, AlarmSource])
            end
        end, Faults),
        ?PRINT(" cached ~p fibers ", [length(Faults)]),
        timer:send_after(30 * 60 * 1000, self(), clean),
        {ok, state};
    {error, Reason} ->
        {stop, Reason}
    end.

handle_call(stop, _From, State) ->
	{stop, normal, ok, State};

handle_call(Request, _From, State) ->
	?ERROR("unexpected requrest: ~n", [Request]),
    {reply, {error, unexpected_request}, State}.




handle_cast({add, Dn, Fiber}, State) ->
    FiberDn = case string:str(to_list(Dn) , "fiber") of
                0 ->
                   "fiber," ++ to_list(Dn);
                _ ->
                    Dn
             end,
    case lookup(FiberDn) of
    {ok, OldFiber} ->
        update_fiber(FiberDn, OldFiber, Fiber);
    false ->
        insert_fiber(FiberDn, Fiber)
    end,
    {noreply, State};

handle_cast({update, FiberDn, Attrs}, State) ->
    case lookup(FiberDn) of
    {ok, OldAttrs} ->
        update_fiber(FiberDn, OldAttrs, Attrs);
    false ->
        ?ERROR("cannot find fiber ~p", [FiberDn])
    end,
    {noreply, State};

handle_cast(Msg, State) ->
	?ERROR("unexpected msg: ~p", [Msg]),
    {noreply, State}.


handle_info(clean, State) ->
%   record(entry, {dn, uid, parent = <<"">>, type, data}).
    mnesia:sync_dirty(fun() ->
        Entries = mnesia:match_object({entry, '_', '_', '_', fiber, '_'}),
        [mnesia:delete_object(Entry) || Entry <- Entries]
    end),
    case emysql:sqlquery("select * from fault_events where
        (alarm_name = 'hwolteponlumlost' or alarm_name = 'hwAlarmonusingledown') and alarm_state <> 3  ") of
    {ok, Faults} ->
        lists:foreach(fun(Fault) ->
            {value, DeviceId} = dataset:get_value(device_id, Fault),
            {value, DeviceType} = dataset:get_value(device_type, Fault),
            {value, AlarmSource} = dataset:get_value(alarm_source, Fault),
            case mit:lookup(to_binary(AlarmSource)) of
            {ok, _Entry} ->
                Uid = get_uid(DeviceType, DeviceId),
                Dn = "fiber," ++ to_list(AlarmSource),
                Data = [{oper_state, 0}, {device_id, DeviceId}, {device_type, DeviceType}],
                ?INFO("mit fiber :~p,~p",[Dn, Data]),
                mit:update(#entry{dn = to_binary(Dn), uid = to_binary(Uid), type = fiber, parent = mit_util:bdn(Dn), data = Data});
            false ->
                io:format("cannot find device: ~p ~n,~p", [DeviceId, AlarmSource])
            end
        end, Faults),
        ?PRINT(" cached ~p fibers ", [length(Faults)]),
        timer:send_after(30 * 60 * 1000, self(), clean);
    {error, Reason} ->
        ?ERROR("~p", [Reason])
    end,
    {noreply, State};

handle_info(Info, State) ->
    ?ERROR("unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



update_fiber(Dn, OldAttrs, Attrs) ->
    ?INFO("update fiber, oldattr: ~p, newattr: ~p", [OldAttrs, Attrs]),
    case mit_util:merge(Attrs, OldAttrs) of
        {changed, MergedAttrs} ->
            {value, DeviceId} = dataset:get_value(device_id, MergedAttrs),
            {value, DeviceType} = dataset:get_value(device_type, MergedAttrs),
            Uid = get_uid(DeviceType, DeviceId),
            mit:update(#entry{dn = Dn, uid = to_binary(Uid), type = fiber, data = MergedAttrs});
        {unchanged, _} ->
            ok
    end.

insert_fiber(Dn, Fiber) ->
    case mit:lookup(mit_util:bdn(Dn)) of
    {ok, #entry{data = _Device} = _} ->
        ?INFO("insert fiber: ~p", [Dn]),
        {value, DeviceId} = dataset:get_value(device_id, Fiber),
        {value, DeviceType} = dataset:get_value(device_type, Fiber),
        Uid = get_uid(DeviceType, DeviceId),
        mit:update(#entry{dn = Dn, uid = to_binary(Uid), type = fiber, data = Fiber});
    false ->
        ?WARNING("cannot find device: ~p", [Dn])
    end.

get_uid(DeviceType, DeviceId) ->
    case DeviceType of
        1 -> "fiber_olt:" ++ integer_to_list(DeviceId);
        2 -> "fiber_onu:" ++ integer_to_list(DeviceId);
        3 -> "fiber_port:" ++ integer_to_list(DeviceId);
        4 -> "fiber_slot:" ++ integer_to_list(DeviceId);
        _ -> "fiber_device:" ++ integer_to_list(DeviceId)
    end.
