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
    case emysql:sql_query(Sql) of
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
    case emysql:sql_query(Sql) of
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

add(Dn, Attrs) ->
    gen_server:cast(?MODULE, {add, Dn, Attrs}).

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
    case  do_init() of
        {ok, State} ->
            {ok, State};
        {error, Reason} ->
            ?ERROR("mit_port start failure...",[]),
            {stop, Reason}
    end.

do_init() ->
    {ok, Ports} = emysql:select(mit_ports, mem_attrs()),
    do_init(Ports),
    {ok, state}.

do_init([]) ->
    ok;
do_init([Port|Ports]) ->
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
                type = port, parent = mit_util:bdn(Dn), data = Port});
        false -> ingore
    end,
    do_init(Ports).

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
    %?INFO("~p,new data :~p", [?MODULE, Attrs]),
    case lookup(Dn) of
        {ok, OldAttrs} ->
            %?INFO("~p,old data :~p", [?MODULE, OldAttrs]),
            update_port(Dn, OldAttrs, Attrs);
        false ->
            ?ERROR("cannot find port: ~p", [Dn])
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
	        {value, CityId} = dataset:get_value(cityid, Entry),
            DevType = mit_util:get_type(Type),
            DateTime = {datetime, calendar:local_time()},
			PortInfo = [{device_type, DevType}, {device_id, Id},{device_manu,DeviceManu},
                         {created_at, DateTime}, {updated_at, DateTime},{cityid,CityId} | Port],
            case emysql:insert(mit_ports, PortInfo) of
                {updated, {0, _}} ->
                    ?WARNING("cannot inserted port: ~p ~p", [Dn, Port]);
                {updated, {1, PId}} ->
                    mit:update(#entry{dn = to_binary(Dn), uid = mit_util:uid(port,PId), type = port,
                        parent = mit_util:bdn(Dn), data = [{id, PId}|Port]});
                {error, Reason} ->
                    ?WARNING("~p", [Reason])
            end;
        false ->
            ?WARNING("cannot find entry: ~p", [Bdn])
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

