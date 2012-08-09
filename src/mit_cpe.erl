
-module(mit_cpe).

-author('chibj.opengoss@gmail.com').

-include("mit.hrl").
-include_lib("elog/include/elog.hrl").

-import(extbif, [to_binary/1]).

-mit_boot_load({cpe, load, "loading cpe", eoc}).

-export([all/0,
         one/1,
         redisco/0
         ]).

%api
-export([attrs/0,
         lookup/1,
         get_entry/1,
         get_notify_entry/1,
		 add/2,
		 update/2]).
     
-import(extbif, [to_list/1]).

all() ->
    Sql = "select  'snmp' means, 'cpe' device_type,t1.* from mit_eoc_terminals t1" ,
    get_data(Sql).

one(Id) ->
    Sql = "select  'snmp' means, 'cpe' device_type,t1.* from mit_eoc_terminals t1 where  t1.id = " ++ to_list(Id),
    get_data(Sql).

redisco() ->
    Sql = "select  'snmp' means, 'cpe' device_type,t1.* from mit_eoc_terminals t1 where  t1.discovery_state = 2",
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
	 eoc_id,
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



get_entry(Cpe) ->
    mit_util:format(mit, mem_attrs(), Cpe).

get_notify_entry(Cpe) ->
    {value, EocId} = dataset:get_value(eoc_id, Cpe),
    {value, Rdn} = dataset:get_value(rdn, Cpe),
    case mit:lookup(id, to_binary("eoc:" ++ integer_to_list(EocId))) of
        {ok, #entry{data = Eoc}} ->
            {value, EocIp} = dataset:get_value(ip, Eoc),
            Dn = to_binary("cpe=" ++ to_list(Rdn) ++ ",eoc=" ++ to_list(EocIp)),
            {value, EocCommunity} = dataset:get_value(snmp_r, Eoc),
            {value, EocVersion} = dataset:get_value(snmp_v, Eoc, <<"v2c">>),
            {value, EocWriteCommunity} = dataset:get_value(snmp_w, Eoc, <<"private">>),
            EocAttrs =  [{eocip, EocIp},{eoc_snmp_r, EocCommunity},{eoc_snmp_v, EocVersion}, {eoc_snmp_w, EocWriteCommunity}],
            Attrs = mit_util:format(notify, attrs(), Cpe),
            EocAttrs ++ [{dn, Dn}|Attrs];
        false ->
            []
    end.


lookup(Dn) ->
    case mit:lookup(Dn) of
        {ok, #entry{data = Cpe}} ->
            {ok, Cpe};
        false ->
            false
    end.


load() ->
    case emysql:select({mit_eoc_terminals, mem_attrs()}) of
        {ok, Cpes} ->
            lists:foreach(fun(Cpe) ->
	              io:format("I want look at cpe: ~p ~n", [Cpe]),
                  {value, Id} = dataset:get_value(id, Cpe),
                  {value, EocId} = dataset:get_value(eoc_id, Cpe),
                  {value, Rdn} = dataset:get_value(rdn, Cpe),
                  case mit:lookup(id, to_binary("eoc:" ++ integer_to_list(EocId))) of
                      {ok, #entry{data = Eoc}} ->
                          {value, EocIp} = dataset:get_value(ip, Eoc),
                          Dn = lists:concat(["cpe=", to_list(Rdn), ",", "eoc=", to_list(EocIp)]),
                          mit:update(#entry{dn = to_binary(Dn), uid = mit_util:uid(cpe,Id), type = cpe,
                              parent = mit_util:bdn(Dn), data = Cpe});
                      false ->
                          ignore
                  end
          end, Cpes),
          io:format("finish start cpe : ~p ~n", [length(Cpes)]),
          {ok, state};
        {error, Reason} ->
            ?ERROR("mit_cpe start failure...~p",[Reason]),
            {ok, state}
    end.

add(Dn, Cpe0) ->
    Cpe = transform(Cpe0),
    case lookup(Dn) of
        {ok, Entry} ->
            update_cpe(Dn, Entry, Cpe);
        false ->
            insert_cpe(Dn, Cpe)
    end.

update(Dn, Attrs) ->
    case lookup(Dn) of
        {ok, OldAttrs} ->
            update_cpe(Dn, OldAttrs, Attrs);
        false ->
            ?ERROR("cannot find cpe ~p", [Dn])
    end.

update_cpe(Dn, OldAttrs, Attrs) ->
    ?INFO("update cpe,dn:~p, oldattr: ~p, newattr: ~p", [Dn, OldAttrs, Attrs]),
    case mit_util:merge(Attrs, OldAttrs) of
        {changed, MergedAttrs} ->
           ?WARNING("update cpe dn:~p,newattr: ~p ~n,result : ~p", [Dn, Attrs, MergedAttrs]),
            {value, Id} = dataset:get_value(id, OldAttrs, -1),
            MergedAttrs1 = lists:keydelete(id, 1, MergedAttrs),
            Datetime = {datetime, calendar:local_time()},
            case emysql:update(mit_eoc_terminals, [{updated_at, Datetime} | MergedAttrs1], {id, Id}) of
                {updated, {1, _Id}} -> %update mit cache
                    mit:update(#entry{dn = Dn, uid = mit_util:uid(cpe,Id), type = cpe, parent = mit_util:bdn(Dn), data = MergedAttrs});
                {updated, {0, _Id}} ->
                    ?WARNING("stale cpe: ~p,~p", [Dn, Id]);
                {error, Reason} ->
                    ?ERROR("~p", [Reason])
            end;
        {unchanged, _} ->
            ok
    end.

insert_cpe(Dn, Cpe) ->
    case mit:lookup(mit_util:bdn(Dn)) of
        {ok, #entry{data = Eoc, type = eoc}} ->
            {value, EocId} = dataset:get_value(id, Eoc),
            {value, CityId} = dataset:get_value(cityid, Eoc),
            {value, Device_manu} = dataset:get_value(device_manu, Eoc),
            {value, DeviceName} = dataset:get_value(device_name, Cpe,""),
            ?INFO("insert cpe: ~p", [Dn]),
            Now = {datetime, calendar:local_time()},
            case emysql:insert(mit_eoc_terminals, [{device_manu,Device_manu},{name,DeviceName},
                {eoc_id, EocId},{cityid, CityId},
                {created_at, Now}, {updated_at, Now}|Cpe]) of
                {updated, {1, Id}} ->
                   ?INFO("insert cpe dn:~p,result: ~p", [Dn, Cpe]),
                    mit:update(#entry{dn = to_binary(Dn), uid = mit_util:uid(cpe,Id), type = cpe, parent = mit_util:bdn(Dn),
                        data = [{id, Id}|Cpe]});
                {updated, {0, _}} ->
                    ?WARNING("cannot find inserted cpe: ~p ~p", [Dn, Cpe]);
                {error, Reason} ->
                    ?ERROR("EocId : ~p,dn :~p, Reason: ~p", [EocId,Dn, Reason]);
                _ ->
                    ok
            end;
        {ok, #entry{type = Type}} ->
            ?ERROR("cannot find :~p to eoc: ~p", [Type, Dn]);
        false ->
            ?ERROR("cannot find eoc: ~p", [Dn])
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
    transform(T, [{device_kind, TypeId},{cpe_type,to_binary(Type)}|Acc]);
transform([H|T], Acc) when is_list(H) ->
    transform(T, [to_binary(H) | Acc]);
transform([H|T], Acc) ->
    transform(T, [H | Acc]).

