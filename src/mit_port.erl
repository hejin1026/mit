%%%----------------------------------------------------------------------
%%% File    : mit_port.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : port of onu or olt
%%% Created : 30 Nov 2009
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2007-2009, www.opengoss.com
%%%----------------------------------------------------------------------
-module(mit_port).

-author('ery.lee@gmail.com').

-import(extbif, [to_binary/1, to_list/1]).

-include("mit.hrl").
-include_lib("elog/include/elog.hrl").

-behavior(gen_server).

-export([start_link/0,
         stop/0]).

-export([all_monet/0,
         one/1
         ]).

%api
-export([get_notify_entry/1,
         lookup/1,
		 add/2,
		 add_ports/2,
		 update_ports/2,
         update/2]).

-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3]).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
	gen_server:call(?SERVER, stop).


all_monet() ->
    Sql = "select t2.means as means, t1.*   from mit_ports t1 LEFT join collect_means t2 on
        (t1.cityid = t2.cityid and t1.device_manu = t2.device_manu) where t2.means is not null and t1.port_category in (1,2)",
    case emysql:sqlquery(Sql) of
        {ok, Records} ->
            Records;
        {error, Reason}  ->
            ?ERROR("~p", [Reason]),
            []
	end.

one(Id) ->
    Sql = "select t2.means as means, t1.*   from mit_ports t1 LEFT join collect_means t2 on
        (t1.cityid = t2.cityid and t1.device_manu = t2.device_manu) where t2.means is not null and t1.port_category in (1,2)" ++
        "and t1.id = " ++ to_list(Id),
    case emysql:sqlquery(Sql) of
        {ok, Records} ->
            Records;
        {error, Reason}  ->
            ?ERROR("~p", [Reason]),
            []
	end.

%% just for monet
mem_attrs() ->
    [id,
     port_name,
     port_index,
     admin_status,
     oper_status,
     port_type,
     device_id,
     device_type,
     slot_no,
     port_no,
     port_category,
     upmaximumbw, %thresh
     downmaximumbw,
     telno,
     ponid
    ].

get_notify_entry(Port) ->
    {value, PortIndex} = dataset:get_value(port_index, Port),
    {value, DevId} = dataset:get_value(device_id, Port),
    {value, DevType} = dataset:get_value(device_type, Port),
    DevUid = mit_util:uid(DevType, DevId),
    Port2 = lists:keydelete(device_type, 1, Port),
    case mit:lookup(id, to_binary(DevUid)) of
        {ok, #entry{dn = DevDn, type = olt, data = Olt}} ->
            {value, OltIp} = dataset:get_value(ip, Olt),
            {value, OltState} = dataset:get_value(olt_state, Olt),
            Rdn = "port=" ++ to_list(PortIndex),
            Dn = Rdn ++ "," ++ binary_to_list(DevDn),
            OltAttrs =  [{oltip, OltIp}, {oper_state, OltState}] ++ get_device_manu(Olt),
            OltAttrs ++ [{dn, list_to_binary(Dn)}, {sub_entry, olt}, {device_type, get_device_type(Port)}|Port2];
        {ok, #entry{dn = DevDn, type = onu, data = Data}} ->
            {value, OnuState} = dataset:get_value(onu_state, Data),
            {value, OnuNo} = dataset:get_value(onuno, Data),
            Rdn = "port=" ++ to_list(PortIndex),
            Dn = Rdn ++ "," ++ binary_to_list(DevDn),
            OltIp = lists:last(string:tokens(binary_to_list(DevDn), "=")),
            OnuAttrs =  [{oltip, OltIp}, {onuno, OnuNo}, {oper_state, OnuState}] ++ get_device_manu(Data),
            OnuAttrs ++ [{dn, list_to_binary(Dn)}, {sub_entry, onu}, {device_type, get_device_type(Port)}|Port2];
        false ->
            ?ERROR("no entry: ~p", [DevUid]),
            []
    end.

get_device_manu(Data) ->
    {value, VendorId} = dataset:get_value(device_manu, Data),
    Vendor =  mit_dict:lookup(vendor, VendorId),
    [{vendor, Vendor}, {device_manu, VendorId}|Data].

get_device_type(Data) ->
    {value, Category} = dataset:get_value(port_category, Data),
    case Category of
        1 -> pon;
        2 -> gei;
        3 -> fei
    end.

lookup(Dn) ->
    case mit:lookup(Dn) of
        {ok, #entry{data = Port}} ->
            {ok, Port};
        false ->
            false
    end.


lookup_from_emysql(Onu) ->
 	{value, DevId} = dataset:get_value(id, Onu),
		case emysql:select({mit_onus, {'and',[{device_id, DevId},{device_type,2}]}}) of
				{ok, []} ->
			          false;
			    {ok, OldPorts} ->
			          {ok,OldPorts};
		     	{error, _} ->
			         ?WARNING("select port error dn:~p",[Onu]),
					 false
				end.

add(Dn, Attrs) ->
    gen_server:cast(?MODULE, {add, Dn, Attrs}).

update_ports(Onu,Ports)->
	gen_server:cast(?MODULE, {update_ports, Onu, Ports}).

add_ports(Onu,Ports)->
	gen_server:cast(?MODULE, {add_ports, Onu, Ports}).

update(Dn, Attrs) ->
    gen_server:cast(?MODULE, {update, Dn, Attrs}).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    case mnesia:system_info(extra_db_nodes) of
        [] -> %master node
            case  do_init() of
                {ok, State} ->
                    io:format("finish start mit port...~n",[]),
					{ok, state};
                {error, Reason} ->
                    ?ERROR("mit_port start failure...",[]),
					{stop, Reason}
            end;
        _ -> %slave node
            {ok, state}
    end.


do_init() ->
    {ok, Ports} = emysql:select({mit_ports, mem_attrs(),{device_type,1}}),
    lists:foreach(fun(Port) ->
        {value, DevId} = dataset:get_value(device_id, Port),
        {value, DevType} = dataset:get_value(device_type, Port),
        DevUid = mit_util:uid(DevType, DevId),
        case mit:lookup(id, to_binary(DevUid)) of
            {ok, #entry{dn = DevDn}} ->
                {value, Id} = dataset:get_value(id, Port),
                {value, PortIndex} = dataset:get_value(port_index, Port),
                Rdn = "port=" ++ to_list(PortIndex),
                Dn = Rdn ++ "," ++ binary_to_list(DevDn),
                mit:update(#entry{dn = to_binary(Dn), uid = mit_util:uid(port,Id),
                    type = port, parent = DevDn, data = Port});
            false -> ingore
        end
    end, Ports),
    {ok, state}.


%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
	{stop, normal, ok, State};

handle_call(Request, _From, State) ->
	?ERROR("unexpected requrest: ~n", [Request]),
    {reply, {error, unexpected_request}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({add, Dn, Port}, State) ->
    case lookup(Dn) of
        {ok, OldPort} ->
            update_port(Dn, OldPort, Port);
        false ->
            insert_port(Dn, Port)
    end,
    {noreply, State};

handle_cast({update, Dn, Attrs}, State) ->
    case lookup(Dn) of
        {ok, OldAttrs} ->
            update_port(Dn, OldAttrs, Attrs);
        false ->
            ?ERROR("cannot find onu ~p", [Dn])
    end,
    {noreply, State};
handle_cast({update_ports, Onu, Ports}, State) ->
    case lookup_from_emysql(Onu) of
        {ok, Records} ->
			OldPorts = [{proplists:get_value(port_index, R), R} || R <- Records],
            batch_update_port(Ports,OldPorts);
        false ->
            ?ERROR("cannot find onu ports ~p", [Onu])
    end,
    {noreply, State};

handle_cast({add_ports, Dn, Ports}, State) ->
 case mit:lookup(Dn) of
        {ok, #entry{type = onu, data = Onu} = _} ->
    		case lookup_from_emysql(Onu) of
        			{ok, Records} ->
						NewPorts = [{proplists:get_value(port_index, R), R} || R <- Ports],
						OldPorts = [{proplists:get_value(port_index, R), R} || R <- Records],
            			batch_insert_port(Onu,NewPorts,OldPorts);
        			false ->
            			?WARNING("cannot find onu ports ~p", [Onu])
    		end;
	    false ->
        	?WARNING("cannot find entry: ~p", [Dn])
end,
    {noreply, State};


handle_cast(Msg, State) ->
	?ERROR("unexpected msg: ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    ?ERROR("unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
insert_port(Dn, Port) ->
    Bdn = mit_util:bdn(Dn),
    case mit:lookup(Bdn) of
        {ok, #entry{type = Type, data = Entry} = _} ->
            % ?INFO("info: insert port dn: ~p, entry: ~p", [Dn, Entry]),
            {value, Id} = dataset:get_value(id, Entry),
            {value, DeviceManu} = dataset:get_value(device_manu, Entry),
            DevType = mit_util:get_type(Type),
            {value, PortIndex} = dataset:get_value(port_index, Port),
            MustInfo = [{device_type, DevType}, {device_id, Id},{device_manu,DeviceManu}],
	        MayInfo = case dataset:get_value(cityid, Entry) of
                {value, CityId} -> [{cityid, CityId}];
                {false, _} -> []
            end,
			PortInfo = MustInfo ++ MayInfo ++  Port,
            case DevType of
                1 ->
                    do_insert_port(Dn, PortInfo);
                _ ->
                    case select_port(Id, DevType, PortIndex) of
                        {ok, []} ->
                            insert_port2(PortInfo);%不写entry
                        {ok, [OldPortInfo]} ->
                            update_port2(OldPortInfo, PortInfo);%不写entry
                        {error, _} ->
                            ?ERROR("select onu port error:~p", [PortInfo])
                    end
            end;
        false ->
            ?WARNING("cannot find entry: ~p", [Bdn])
    end.

select_port(Id, DevType, PortIndex) ->
    emysql:select({mit_ports, {'and', {'and', {port_index, PortIndex}, {device_type, DevType}}, {device_id, Id}}}).

do_insert_port(Dn, PortInfo) ->
    case emysql:insert(mit_ports, [{created_at, {datetime, calendar:local_time()}}| PortInfo]) of
        {updated, {0, _}} ->
            ?WARNING("cannot inserted port: ~p ~p", [PortInfo]);
        {updated, {1, PId}} ->
            mit:update(#entry{dn = to_binary(Dn), uid = mit_util:uid(port,PId), type = port,
                parent = mit_util:bdn(Dn), data = [{id, PId}|PortInfo]}),
            add_splite(Dn,[{id, PId}|PortInfo]); %每加入一个PON口，同时生成一个一级分光器，直接挂在pon下
        {error, Reason} ->
            ?WARNING("~p", [Reason])
    end.

%不写entry
insert_port2(PortInfo) ->
    case emysql:insert(mit_ports, PortInfo) of
        {updated, _} ->
            ok;
        {error, Reason} ->
            ?WARNING("~p", [Reason])
    end.

update_port(Dn, OldAttrs, Attrs) ->
    case mit_util:merge(Attrs, OldAttrs) of
        {changed, MergedAttrs} ->
            {value, Id} = dataset:get_value(id, OldAttrs),
            % ?WARNING("info :update port: ~p, ~p", [Dn, MergedAttrs]),
            Datetime = {datetime, calendar:local_time()},
            MergedAttrs1 = lists:keydelete(id, 1, MergedAttrs),
            MergedAttrs2 = lists:keydelete(means, 1, MergedAttrs1),
            case emysql:update(mit_ports, [{updated_at, Datetime} | MergedAttrs2], {id, Id}) of
                {updated, {1, _Id}} -> %update mit cache
                    mit:update(#entry{dn = Dn, uid = mit_util:uid(port,Id), type = port,
                        parent = mit_util:bdn(Dn), data = MergedAttrs});
                {updated, {0, _}} -> %stale port?
                    ?WARNING("stale port: ~p", [Dn]);
                {error, Reason} ->
                    ?ERROR("~p", [Reason])
            end;
        {unchanged, _} ->
            ok
    end.


%不写entry
update_port2(OldAttrs, Attrs) ->
    case mit_util:merge(Attrs, OldAttrs) of
        {changed, MergedAttrs} ->
            {value, Id} = dataset:get_value(id, OldAttrs),
            % ?WARNING("info :update port: ~p, ~p", [Dn, MergedAttrs]),
            Datetime = {datetime, calendar:local_time()},
            MergedAttrs1 = lists:keydelete(id, 1, MergedAttrs),
            MergedAttrs2 = lists:keydelete(means, 1, MergedAttrs1),
            case emysql:update(mit_ports, [{updated_at, Datetime} | MergedAttrs2], {id, Id}) of
                {updated, _} ->
                    ok;
                {error, Reason} ->
                    ?ERROR("~p", [Reason])
            end;
        {unchanged, _} ->
            ok
    end.

add_splite(PonDn,Port)->
	?INFO("add_splite ~p,~p", [PonDn,Port]),
	{value, DeviceType} = dataset:get_value(device_type, Port,0),
	{value, PortCategory} = dataset:get_value(port_category, Port,0),
	{value, PonId} = dataset:get_value(id, Port),
	{value, OltId} = dataset:get_value(device_id, Port),
	{value, PortName} = dataset:get_value(port_name, Port),
	if DeviceType==1 andalso PortCategory==1 ->
			mit_splite:add(PonDn,[{pon_id,PonId},{olt_id,OltId},{split_name,PortName},{splitter_level,1}]);
		true -> ignore
	end.

batch_insert_port(Onu,NewPorts,OldPorts)->
	DeviceManu = proplists:get_value(device_manu, Onu,0),
		CityId = proplists:get_value(cityid, Onu,0),
		OnuId = proplists:get_value(id, Onu,0),
	NewIdxList = [Idx || {Idx, _} <- NewPorts],
	OldIdxList = [Idx || {Idx, _} <- OldPorts],
	{Added, Updated, Deleted} = extlib:list_compare(NewIdxList, OldIdxList),
	%added
	lists:foreach(fun(Idx) ->
	MustInfo = [{device_type, 2}, {device_id, OnuId},{device_manu,DeviceManu},{cityid,CityId}],
	NewPort = proplists:get_value(Idx, NewPorts),
	case emysql:insert(mit_ports, MustInfo++NewPort) of
		{error, Err} -> ?ERROR("insert port error ~p", [Err]);
		_ -> ok
	end
	end, Added),
	%updated
	lists:foreach(fun(Idx) ->
	NewPort = proplists:get_value(Idx, NewPorts),
	OldPort = proplists:get_value(Idx, OldPorts),
	update_port2(OldPort, NewPort)
	end, Updated),
	%deleted
	?WARNING("find deleted ports and do nothing. onu ~p,index~p",[Onu,Deleted]).


batch_update_port([], _OldPorts) ->
		    ok;

batch_update_port([Port|T], OldPorts) ->
    case dataset:get_value(port_index, Port,false) of
    {value, false} ->
        ?WARNING("no index in port ~p", [Port]);
    {ok, PortIndex} ->
		case proplists:get_value(PortIndex, OldPorts,false) of
			false -> ?WARNING("can not find port ~p,index:~p", [Port,PortIndex]);
			OldPort -> update_port2(OldPort,Port)
		end
    end,
    batch_update_port(T,OldPorts).














