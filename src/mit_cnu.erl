
-module(mit_cnu).

-author('chibj.opengoss@gmail.com').

-include("mit.hrl").
-include_lib("elog/include/elog.hrl").

-import(extbif, [to_binary/1]).

-mit_boot_load({cnu, load, "loading cnu", clt}).

-export([all/0,
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
		 update/2]).

-import(extbif, [to_list/1]).

all() ->
    Sql = "select  'snmp' means, 'cnu' device_type,t1.* from mit_cnus t1" ,
    get_data(Sql).

one(Id) ->
    Sql = "select  'snmp' means, 'cnu' device_type,t1.* from mit_cnus t1 where  t1.id = " ++ to_list(Id),
    get_data(Sql).

redisco() ->
    Sql = "select  'snmp' means, 'cnu' device_type,t1.* from mit_cnus t1 where  t1.discovery_state = 2",
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
	 clt_id,
     sysoid,
     device_name,
	 terminal_status,
     mac,
     serial_no,
     hardware_version,
     software_version,
     radio_channe,
     oper_mode,
     discovery_state,
     device_kind,
     device_manu,
	 rdn,
     collect_status,
     snmp_r,
     snmp_w,
     snmp_v
    ].



get_entry(Cnu) ->
    mit_util:format(mit, mem_attrs(), Cnu).

get_notify_entry(Cnu) ->
    {value, CltId} = dataset:get_value(clt_id, Cnu),
    {value, Rdn} = dataset:get_value(rdn, Cnu),
    case mit:lookup(id, to_binary("clt:" ++ integer_to_list(CltId))) of
        {ok, #entry{data = Clt}} ->
            {value, CltIp} = dataset:get_value(ip, Clt),
            Dn = to_binary("cnu=" ++ to_list(Rdn) ++ ",clt=" ++ to_list(CltIp)),
            {value, CltCommunity} = dataset:get_value(snmp_r, Clt),
            {value, CltVersion} = dataset:get_value(snmp_v, Clt, <<"v2c">>),
            {value, CltWriteCommunity} = dataset:get_value(snmp_w, Clt, <<"private">>),
            CltAttrs =  [{cltip, CltIp},{clt_snmp_r, CltCommunity},{clt_snmp_v, CltVersion}, {clt_snmp_w, CltWriteCommunity}],
            Attrs = mit_util:format(notify, attrs(), Cnu),
            CltAttrs ++ [{dn, Dn}|Attrs];
        false ->
            []
    end.


lookup(Dn) ->
    case mit:lookup(Dn) of
        {ok, #entry{data = Cnu}} ->
            {ok, Cnu};
        false ->
            false
    end.


load() ->
    case emysql:select({mit_cnus, mem_attrs()}) of
        {ok, Cnus} ->
            lists:foreach(fun(Cnu) ->
	              %io:format("I want look at cnu: ~p ~n", [Cnu]),
                  {value, Id} = dataset:get_value(id, Cnu),
                  {value, CltId} = dataset:get_value(clt_id, Cnu),
                  {value, Rdn} = dataset:get_value(rdn, Cnu),
                  case mit:lookup(id, to_binary("clt:" ++ integer_to_list(CltId))) of
                      {ok, #entry{data = Clt}} ->
                          {value, CltIp} = dataset:get_value(ip, Clt),
                          Dn = lists:concat(["cnu=", to_list(Rdn), ",", "clt=", to_list(CltIp)]),
                          mit:update(#entry{dn = to_binary(Dn), uid = mit_util:uid(cnu,Id), type = cnu,
                              parent = mit_util:bdn(Dn), data = Cnu});
                      false ->
                          ignore
                  end
          end, Cnus),
          io:format("finish start cnu : ~p ~n", [length(Cnus)]),
          {ok, state};
        {error, Reason} ->
            ?ERROR("mit_cnu start failure...~p",[Reason]),
            {ok, state}
    end.

add(Dn, Cnu0) ->
    Cnu = transform(Cnu0),
    case lookup(Dn) of
        {ok, Entry} ->
            update_cnu(Dn, Entry, Cnu);
        false ->
            insert_cnu(Dn, Cnu)
    end.

update(Dn, Attrs) ->
    case lookup(Dn) of
        {ok, OldAttrs} ->
            update_cnu(Dn, OldAttrs, Attrs);
        false ->
            ?ERROR("cannot find cnu ~p", [Dn])
    end.

update_cnu(Dn, OldAttrs, Attrs) ->
    ?INFO("update cnu,dn:~p, oldattr: ~p, newattr: ~p", [Dn, OldAttrs, Attrs]),
    case mit_util:merge(Attrs, OldAttrs) of
        {changed, MergedAttrs,_} ->
           ?WARNING("update cnu dn:~p,newattr: ~p ~n,result : ~p", [Dn, Attrs, MergedAttrs]),
            {value, Id} = dataset:get_value(id, OldAttrs, -1),
            MergedAttrs1 = lists:keydelete(id, 1, MergedAttrs),
            Datetime = {datetime, calendar:local_time()},
            case emysql:update(mit_cnus, [{updated_at, Datetime} | MergedAttrs1], {id, Id}) of
                {updated, {1, _Id}} -> %update mit cache
                    mit:update(#entry{dn = Dn, uid = mit_util:uid(cnu,Id), type = cnu, parent = mit_util:bdn(Dn), data = MergedAttrs});
                {updated, {0, _Id}} ->
                    ?WARNING("stale cnu: ~p,~p", [Dn, Id]);
                {error, Reason} ->
                    ?ERROR("~p", [Reason])
            end;
        {unchanged, _,_} ->
            ok
    end.

insert_cnu(Dn, Cnu) ->
    case mit:lookup(mit_util:bdn(Dn)) of
        {ok, #entry{data = Clt, type = clt}} ->
            {value, CltId} = dataset:get_value(id, Clt),
            {value, CityId} = dataset:get_value(cityid, Clt),
            {value, Device_manu} = dataset:get_value(device_manu, Clt),
            {value, DeviceName} = dataset:get_value(device_name, Cnu,""),
            ?INFO("insert cnu: ~p", [Dn]),
            Now = {datetime, calendar:local_time()},
            case emysql:insert(mit_cnus, [{device_manu,Device_manu},{name,DeviceName},
                {clt_id, CltId},{cityid, CityId},
                {created_at, Now}, {updated_at, Now}|Cnu]) of
                {updated, {1, Id}} ->
                   ?INFO("insert cnu dn:~p,result: ~p", [Dn, Cnu]),
                    mit:update(#entry{dn = to_binary(Dn), uid = mit_util:uid(cnu,Id), type = cnu, parent = mit_util:bdn(Dn),
                        data = [{id, Id}|Cnu]});
                {updated, {0, _}} ->
                    ?WARNING("cannot find inserted cnu: ~p ~p", [Dn, Cnu]);
                {error, Reason} ->
                    ?ERROR("CltId : ~p,dn :~p, Reason: ~p", [CltId,Dn, Reason]);
                _ ->
                    ok
            end;
        {ok, #entry{type = Type}} ->
            ?ERROR("cannot find :~p to clt: ~p", [Type, Dn]);
        false ->
            ?ERROR("cannot find clt: ~p", [Dn])
    end.

transform(Attrs) ->
    transform(Attrs, []).

transform([], Acc) ->
    Acc;
transform([{ip, Ip} | T], Acc) ->
    Ip1 = to_list(Ip),
    if Ip1 == "0.0.0.0" ->
            transform(T, Acc);
        Ip1 == "255.255.255.255" ->
            transform(T, Acc);
        true ->
            transform(T, [{ip, Ip}|Acc])
    end;
transform([{vendor, Vendor}|T], Acc) ->
    ManuId = mit_dict:lookup(vendor, Vendor),
    transform(T, [{device_manu, ManuId}|Acc]);
transform([{type,Type }|T], Acc) ->
    TypeId = mit_dict:lookup(type, Type),
    transform(T, [{device_kind, TypeId},{cnu_type,to_binary(Type)}|Acc]);
transform([H|T], Acc) when is_list(H) ->
    transform(T, [to_binary(H) | Acc]);
transform([H|T], Acc) ->
    transform(T, [H | Acc]).

