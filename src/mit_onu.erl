
-module(mit_onu).

-created("hejin 2012-8-6").

-include("mit.hrl").
-include_lib("elog/include/elog.hrl").

-mit_boot_load({onu, load, "loading onu", olt}).

-import(extbif, [to_binary/1]).

-export([all/0,
        snmp_all/0,
         one/1,
         redisco/0
         ]).

%api
-export([load/0,
        attrs/0,
         lookup/1,
         get_entry/1,
         get_notify_entry/1,
		 add/2,
         add_onus/2,
		 update/2,
		 update_onus/2
		]).

-import(extbif, [to_list/1]).


snmp_all() ->
    Sql = "select t2.means as means, t1.* ,'onu' device_type  from mit_onus t1 LEFT join collect_means t2 on
        (t1.cityid = t2.cityid and t1.device_manu = t2.device_manu) where t2.means is not null and t1.collect_type=2  and t1.onu_state < 2",
    get_data(Sql).

all() ->
    Sql = "select t2.means as means, t1.* ,'onu' device_type  from mit_onus t1 LEFT join collect_means t2 on
        (t1.cityid = t2.cityid and t1.device_manu = t2.device_manu) where t2.means is not null   and t1.onu_state < 2",
    get_data(Sql).

one(Id) ->
    Sql = "select t2.means as means, t1.* ,'onu' device_type  from mit_onus t1 LEFT join collect_means t2 on
        (t1.cityid = t2.cityid and t1.device_manu = t2.device_manu) where t2.means is not null and t1.id = " ++ to_list(Id),
    get_data(Sql).

redisco() ->
    Sql = "select t2.means as means, t1.*,'onu' device_type   from mit_onus t1 LEFT join collect_means t2 on
        (t1.cityid = t2.cityid and t1.device_manu = t2.device_manu) where t2.means is not null and t1.discovery_state = 2",
    get_data(Sql).

get_data(Sql) ->
    case emysql:sqlquery(Sql) of
        {ok, Records} ->
            Records;
        {error, Reason}  ->
            ?ERROR("~p", [Reason]),
            []
	end.


attrs() ->
    [
     means,
     device_type
    ] ++ mem_attrs().

mem_attrs() ->
    [id,
     ip,
     cityid,
     sysoid,
	 splite_id,
     device_name,
     adminstate,
     operstate,
     authmacaddr,
     authmacsn,
     collect_type,
     discovery_state,
     device_kind,
     device_manu,
     macaddr,
	 ping_status,
	 snmp_status,
     olt_id,
     slot_no,
     port_no,
     onu_no,
     onu_state,
     onu_type,
     pvlan,
     rdn,
     upmaximumbw,
     downmaximumbw,
     entrance_id,
     ponid,
     snmp_r,
     snmp_w,
     snmp_v
    ].


get_entry(Onu) ->
    mit_util:format(mit, mem_attrs(), Onu).

get_notify_entry(Onu) ->
    {value, OltId} = dataset:get_value(olt_id, Onu),
    {value, Rdn} = dataset:get_value(rdn, Onu),
    case mit:lookup(id, to_binary("olt:" ++ integer_to_list(OltId))) of
        {ok, #entry{data = Olt}} ->
            {value, OltIp} = dataset:get_value(ip, Olt),
            Dn = to_binary("onu=" ++ to_list(Rdn) ++ ",olt=" ++ to_list(OltIp)),
            {value, OltCommunity} = dataset:get_value(snmp_r, Olt),
            {value, OltVersion} = dataset:get_value(snmp_v, Olt, <<"v2c">>),
            {value, OltWriteCommunity} = dataset:get_value(snmp_w, Olt, <<"private">>),
            OltAttrs =  [{oltip, OltIp},{olt_snmp_r, OltCommunity},{olt_snmp_v, OltVersion}, {olt_snmp_w, OltWriteCommunity}],
            Attrs = mit_util:format(notify, attrs(), Onu),
            OltAttrs ++ [{dn, Dn}|Attrs];
        false ->
            []
    end.


lookup(Dn) ->
    case mit:lookup(Dn) of
        {ok, #entry{data = Onu}} ->
            {ok, Onu};
        false ->
            false
    end.

load() ->
    case emysql:sqlquery("select t.ip as olt_ip,o.* from mit_onus o LEFT join mit_olts t on t.id = o.olt_id ") of
        {ok, Onus} ->
            ?ERROR("start mem onu ~n", []),
            lists:foreach(fun(Onu) ->
                  {value, Id} = dataset:get_value(id, Onu),
                  {value, OltIp} = dataset:get_value(olt_ip, Onu),
                  Entry = case dataset:get_value(ip, Onu) of
                      {value, Ip} ->
                          #entry{uid = mit_util:uid(onu,Id), ip=Ip, type = onu, data = get_entry(Onu)};
                      {false, _} ->
                          #entry{uid = mit_util:uid(onu,Id), type = onu, data = get_entry(Onu)}
                   end,
                  {value, Rdn} = dataset:get_value(rdn, Onu),
                  OltDn = lists:concat(["olt=", to_list(OltIp)]),
                  Dn = lists:concat(["onu=", to_list(Rdn),",", OltDn]),
                  mit:update(Entry#entry{dn = list_to_binary(Dn), parent = list_to_binary(OltDn)})
          end, Onus),
          ?ERROR("finish start onu : ~p ~n", [length(Onus)]);
        {error, Reason} ->
            ?ERROR("mit_onu start failure...~p",[Reason])
    end.

get_dn(OltIp, Onu) ->
      {value, Rdn} = dataset:get_value(rdn, Onu),
      list_to_binary(lists:concat(["onu=", to_list(Rdn),",olt=", to_list(OltIp)])).

get_dn2(OltDn, Rdn) ->
      list_to_binary(lists:concat(["onu=", to_list(Rdn),",", to_list(OltDn)])).

add(Dn, Onu0) ->
    Onu = transform(Onu0),
    case lookup(Dn) of
        {ok, OldOnu} ->
            update_onu(Dn, OldOnu, Onu);
        false ->
            insert_onu(to_binary(Dn), Onu)
    end.

update(Dn, Attrs) ->
    case lookup(Dn) of
        {ok, OldAttrs} ->
            update_onu(Dn, OldAttrs, Attrs);
        false ->
            ?ERROR("cannot find onu ~p", [Dn])
    end.

add_onus(OltDn, Onus) ->
    case mit:lookup(OltDn) of
        {ok, #entry{uid = OltId, data = Olt}} ->
            {ok, OnusInDb} = emysql:select(mit_onus,{olt_id, mit_util:nid(OltId)}),
            OnuList = [{to_binary(proplists:get_value(rdn,Onu)), transform(Onu)} || Onu <- Onus],
            OnuDbList = [{to_binary(proplists:get_value(rdn,Onu)), get_entry(Onu)} || Onu <- OnusInDb],
            {AddList, UpdateList, _DelList} = extlib:list_compare(mit_util:get_key(OnuList), mit_util:get_key(OnuDbList)),
            [insert_onu(Olt, proplists:get_value(Rdn, OnuList)) || Rdn <- AddList],
            [update_onu(get_dn2(OltDn, Rdn), proplists:get_value(Rdn, OnuDbList), proplists:get_value(Rdn, OnuList)) ||
                Rdn <- UpdateList];
         _ ->
             ignore
     end.


update_onus(OltDn, Onus) ->
	lists:foreach(fun(Onu) ->
		Rdn = proplists:get_value(rdn,Onu,""),
		OnuDn = get_dn2(OltDn, Rdn),
	    case mit:lookup(OnuDn) of
	        {ok, #entry{dn = _Dn, type = onu, data = OldOnu}} ->
	            update_onu(OnuDn, OldOnu, Onu);
	      	_ ->
	            ?WARNING("cannot find onu ~p,~p", [OltDn,Onu])
	     end
		end,Onus).


insert_onu(Dn, Onu) when is_binary(Dn) ->
    case mit:lookup(mit_util:bdn(Dn)) of
        {ok, #entry{data = Olt, type = olt}} ->
            insert_onu(Olt, Onu);
        {ok, #entry{type = Type}} ->
            ?ERROR("cannot find :~p to olt: ~p", [Type, Dn]);
        false ->
            ?ERROR("cannot find olt: ~p", [Dn])
    end;
insert_onu(Olt, Onu) when is_list(Olt) ->
    {value, OltId} = dataset:get_value(id, Olt),
    {value, OltIp} = dataset:get_value(ip, Olt),
    {value, CityId} = dataset:get_value(cityid, Olt),
    {value, DeviceName} = dataset:get_value(device_name, Onu,""),
    Now = {datetime, calendar:local_time()},
    case emysql:insert(mit_onus, [{olt_id, OltId},{cityid, CityId},{name,DeviceName},{created_at, Now}|Onu]) of
        {updated, {1, Id}} ->
       %     ?INFO("insert onu dn:~p,result: ~p", [Dn, Onu]),
            {value, Ip} = dataset:get_value(ip, Onu, undefined),
            Dn = get_dn(OltIp, Onu),
            mit:update(#entry{dn = to_binary(Dn), uid = mit_util:uid(onu,Id),ip=Ip,type = onu,
                parent = mit_util:bdn(Dn),data = [{id, Id},{olt_id, OltId},{cityid, CityId},{created_at, Now}|Onu]});
        {updated, {0, _}} ->
            ?WARNING("cannot find inserted onu: ~p ~p",  [Onu]);
        {error, Reason} ->
            ?ERROR("OltId : ~p,dn :~p, ~nReason: ~p", [OltId, Onu, Reason]);
        _ ->
            ok
    end.

update_onu(Dn, OldAttrs, NewAttrs) ->
    %?INFO("update onu,dn:~p, oldattr: ~p, newattr: ~p", [Dn, OldAttrs, Attrs]),
    case mit_util:merge(NewAttrs, OldAttrs) of
        {changed, MergedAttrs0,_} ->
           % ?WARNING("update onu dn:~p,newattr: ~p ~n,result : ~p", [Dn, Attrs, MergedAttrs]),
			MergedAttrs = case dataset:get_value(device_manu, OldAttrs, false) of
							{value, 2001} -> do_operstart_for_huawei(OldAttrs,MergedAttrs0);
							_  	-> MergedAttrs0
						   end,
            Datetime = {datetime, calendar:local_time()},
            case emysql:update(mit_onus, [{updated_at, Datetime} | MergedAttrs]) of
                {updated, {1, _}} -> %update mit cache
					{value, Id} = dataset:get_value(id, OldAttrs),
                    {value, Ip} = dataset:get_value(ip, MergedAttrs, undefined),
                    mit:update(#entry{dn = to_binary(Dn), uid = mit_util:uid(onu,Id), ip = Ip,
                        type = onu, parent = mit_util:bdn(Dn), data = MergedAttrs});
                {updated, {0, Id}} -> %stale onu?
                    ?WARNING("stale onu: ~p,~p", [Dn, Id]);
                {error, Reason} ->
                    ?ERROR("~p", [Reason])
            end;
        {unchanged, _,_} ->
            ok
    end.

do_operstart_for_huawei(OldAttrs,MergedAttrs0) ->
	 ?INFO("do_operstart_for_huawei OldAttrs:~p~n,MergedAttrs: ~p ~n",  [OldAttrs, MergedAttrs0]),
	{value, OldOper} = dataset:get_value(operstate, OldAttrs, 0),
    {value, NewOper} = dataset:get_value(operstate, MergedAttrs0,1),
	if NewOper==3 andalso OldOper==2 ->
		 ?WARNING("find exception operstate when update onu. OldAttrs:~p~n,MergedAttrs: ~p ~n",  [OldAttrs, MergedAttrs0]),
		lists:keyreplace(operstate, 1, MergedAttrs0, {operstate, 2});
		true-> MergedAttrs0
	end.







transform(Attrs) ->
    transform(Attrs, []).

transform([], Acc) ->
    Acc;
transform([{ip, Ip} | T], Acc) ->
    Ip1 = to_list(Ip),
    if Ip1 == "0.0.0.0" ->
            transform(T, [{collect_type,1}|Acc]);
       Ip1 == "255.255.255.255" ->
            transform(T,  [{collect_type,1}|Acc]);
       Ip1 == "" ->
            transform(T,  [{collect_type,1}|Acc]);
       Ip1 == "--" ->
            transform(T,  [{collect_type,1}|Acc]);
        true ->
            transform(T, [{ip, Ip},{collect_type,2}|Acc])
    end;
transform([{vendor, Vendor}|T], Acc) ->
    ManuId = mit_dict:lookup(vendor, Vendor),
    transform(T, [{device_manu, ManuId}|Acc]);
transform([{type,Type }|T], Acc) ->
    TypeId = mit_dict:lookup(type, Type),
    transform(T, [{device_kind, TypeId},{onu_type,to_binary(Type)}|Acc]);
transform([H|T], Acc) when is_list(H) ->
    transform(T, [to_binary(H) | Acc]);
transform([H|T], Acc) ->
    transform(T, [H | Acc]).

