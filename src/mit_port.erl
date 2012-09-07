-module(mit_port).

-created("hejin 2012-8-7").

-import(extbif, [to_binary/1, to_list/1]).

-include("mit.hrl").
-include_lib("elog/include/elog.hrl").

-mit_boot_load({port, load, "loading olt port", olt}).

-export([all_monet/0,
         one/1
         ]).

%api
-export([load/0,
        get_notify_entry/1,
         lookup/1,
		 add/2,
		 add_ports/2,
		 update_ports/2,
         update/2]).

-define(SERVER, ?MODULE).

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

load() ->
    ?ERROR("look mem port ...", []),
    {ok, Ports} = emysql:sqlquery("select t.ip as olt_ip,p.*
        from mit_ports p LEFT join mit_olts t on t.id = p.device_id  where p.device_type = 1"),
    ?ERROR("start mem port ...", []),
    lists:foreach(fun(Port) ->
        {value, OltIp} = dataset:get_value(olt_ip, Port),
        OltDn = lists:concat(["olt=", to_list(OltIp)]),
        {value, Id} = dataset:get_value(id, Port),
        {value, PortIndex} = dataset:get_value(port_index, Port),
        Rdn = "port=" ++ to_list(PortIndex),
        Dn = Rdn ++ "," ++ OltDn,
        mit:update(#entry{dn = to_binary(Dn), uid = mit_util:uid(port,Id),
            type = port, parent = to_binary(OltDn), data = mit_util:format(mit, mem_attrs(), Port)})
    end, Ports),
    io:format("finish start port ~p...~n",[length(Ports)]).


add(Dn, Port) ->
    case lookup(Dn) of
        {ok, OldPort} ->
            update_port(Dn, OldPort, Port);
        false ->
            insert_port(Dn, Port)
    end.

update(Dn, Attrs) ->
    case lookup(Dn) of
        {ok, OldAttrs} ->
            update_port(Dn, OldAttrs, Attrs);
        false ->
            ?ERROR("cannot find onu ~p", [Dn])
    end.


add_ports(Dn, Ports) ->
    do_ports(Dn, Ports, fun({AddList, UpdateList}, Type,Entry, DbList, List) ->
        [insert_port(Type, Entry, proplists:get_value(Rdn, List)) || Rdn <- AddList],
        [update_port(proplists:get_value(Rdn, DbList), proplists:get_value(Rdn, List)) || Rdn <- UpdateList]
    end).

update_ports(Dn, Ports) ->
    do_ports(Dn, Ports, fun({_AddList, UpdateList},_Onu, DbList, List) ->
        [update_port(proplists:get_value(Rdn, DbList), proplists:get_value(Rdn, List)) || Rdn <- UpdateList]
    end).

do_ports(Dn, Ports, Callback) ->
    case mit:lookup(Dn) of
	        {ok, #entry{uid= Id, type = Type, data = Onu} = _} ->
	    		{ok, PortInDb} = emysql:select({mit_ports, {'and',[{device_id, mit_util:nid(Id)},{device_type,mit_util:get_type(Type)}]}}),
                List = [{to_binary(proplists:get_value(port_index, R)), R} || R <- Ports],
                DbList = [{to_binary(proplists:get_value(port_index, R)), R} || R <- PortInDb],
                {AddList, UpdateList, _DelList} = extlib:list_compare(mit_util:get_key(List), mit_util:get_key(DbList)),
                Callback({AddList, UpdateList},Type, Onu, DbList, List);
		    false ->
	        	?WARNING("cannot find entry: ~p", [Dn])
	end.

insert_port(Dn, Port) ->
    Bdn = mit_util:bdn(Dn),
    case mit:lookup(Bdn) of
        {ok, #entry{type = Type, data = Entry} = _} ->
            InsertMem = fun(Id, PortInfo) ->
                 mit:update(#entry{dn = Dn, uid = mit_util:uid(port,Id), type = port,
                     parent = mit_util:bdn(Dn), data = PortInfo}),
                 add_splite(Dn,[{id, Id}|PortInfo]) %每加入一个PON口，同时生成一个一级分光器，直接挂在pon下
             end,
            do_insert(Type, Entry, Port, InsertMem);
         false ->
            ?WARNING("cannot find entry: ~p", [Bdn])
    end.

insert_port(Type, Entry, Port) ->
    do_insert(Type, Entry, Port, ingore).

do_insert(Type, Entry, Port, Callback) ->
    % ?INFO("info: insert port dn: ~p, entry: ~p", [Dn, Entry]),
    PortInfo = get_device_info(Type, Entry) ++  Port,
    case emysql:insert(mit_ports, [{created_at, {datetime, calendar:local_time()}}| PortInfo]) of
        {updated, {0, _}} ->
            ?WARNING("cannot inserted type: ~p ~p", [Type, PortInfo]);
        {updated, {1, PId}} ->
            if is_function(Callback) ->
                Callback(PId, PortInfo);
             true ->
                 ok
            end;
        {error, Reason} ->
            ?WARNING("~p", [Reason])
    end.

get_device_info(Type, Entry) ->
    {value, Id} = dataset:get_value(id, Entry),
    {value, CityId} = dataset:get_value(cityid, Entry,0),
    {value, DeviceManu} = dataset:get_value(device_manu, Entry,0),
    DeviceType = mit_util:get_type(Type),
    [{device_type, DeviceType}, {device_id, Id},{device_manu,DeviceManu},{cityid,CityId}].



update_port(Dn, OldAttrs, Attrs) ->
    UpdateMem = fun(Id, PortInfo) ->
         mit:update(#entry{dn = Dn, uid = mit_util:uid(port,Id), type = port, parent = mit_util:bdn(Dn), data = PortInfo})
     end,
    mit_util:do_update(mit_ports, Attrs, OldAttrs, UpdateMem).

update_port(OldAttrs, Attrs) ->
    mit_util:do_update(mit_ports, Attrs, OldAttrs, ignore).


add_splite(PonDn,Port)->
	?INFO("add_splite ~p,~p", [PonDn,Port]),
	{value, DeviceType} = dataset:get_value(device_type, Port,0),
	{value, PortCategory} = dataset:get_value(port_category, Port,0),
	if DeviceType==1 andalso PortCategory==1 ->
            {value, PonId} = dataset:get_value(id, Port),
            {value, OltId} = dataset:get_value(device_id, Port),
            {value, PortName} = dataset:get_value(port_name, Port),
			mit_splite:add(PonDn,[{pon_id,PonId},{olt_id,OltId},{split_name,PortName},{splitter_level,1}]);
		true -> ignore
	end.

