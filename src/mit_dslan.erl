-module(mit_dslan).

-create("chibj 2012-9-7").

-include("mit.hrl").
-include_lib("elog/include/elog.hrl").

-import(extbif, [to_binary/1, to_list/1]).

-mit_boot_load({dslan, load, "loading dslan", undefined}).

-export([all/0,
         one/1,
         redisco/0
         ]).

-export([load/0,
        attrs/0,
         lookup/1,
         get_entry/1,
         get_notify_entry/1,
         add/2,
         update/2]).

-define(SERVER, ?MODULE).

all() ->
    Sql = "select  'snmp' means, 'dslan' device_type,t1.* from mit_dslans t1" ,
    get_data(Sql).

one(Id) ->
    Sql = "select  'snmp' means, 'dslan' device_type, t1.* from mit_dslans t1 where  t1.id = " ++ to_list(Id),
    get_data(Sql).

redisco() ->
    Sql = "select  'snmp' means, 'dslan' device_type, t1.* from mit_dslans t1 where  t1.discovery_state = 2",
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
     cityid,
     branch_id,
     sysoid,
     ip,
     mask,
     mac,
	 ping_status,
	 snmp_status,
     device_manu,
     device_kind,
     is_discovery,
     discovery_state,
     collect_status,
     snmp_r,
     snmp_w,
     snmp_v
    ].


get_entry(Dslan) ->
    mit_util:format(mit, mem_attrs(), Dslan).

get_notify_entry(Dslan) ->
    {value, Ip} = dataset:get_value(ip, Dslan),
	Dn = "dslan=" ++ to_list(Ip),
    Attrs = mit_util:format(notify, attrs(), Dslan),
     [{dn, list_to_binary(Dn)}|Attrs].


lookup(Dn) ->
    case mit:lookup(Dn) of
    {ok, #entry{data = Dslan}} ->
        {ok, Dslan};
    false ->
        false
    end.

load() ->
    case emysql:select({mit_dslans, mem_attrs()}) of
        {ok, Dslans} ->
            lists:foreach(fun(Dslan) ->
                {value, Id} = dataset:get_value(id, Dslan),
                {value, Ip} = dataset:get_value(ip, Dslan),
                Dn = "dslan=" ++ to_list(Ip),
                Entry = #entry{dn = to_binary(Dn), uid = mit_util:uid(dslan,Id),ip=Ip, type = dslan, parent = undefined, data = Dslan},
                mit:update(Entry)
            end, Dslans),
            io:format("finish start dslan : ~p ~n", [length(Dslans)]),
            {ok, state};
        {error, Reason} ->
            ?ERROR("start failure...~p",[Reason]),
            {stop, Reason}
    end.

add(Dn, Attrs) ->
    Dslan = transform(Attrs),
    case mit:lookup(Dn) of
        {ok, #entry{data = Entry} = _} ->
            update_dslan(Dn, Entry, Dslan);
        false ->
            insert_dslan(Dn, Dslan)
    end.

update(Dn, Attrs) ->
    case mit:lookup(Dn) of
    {ok, #entry{data = Entry} = _} ->
        update_dslan(Dn, Entry, Attrs);
    false ->
        ?ERROR("cannot find dslan ~p", [Dn])
    end.

insert_dslan(Dn, Dslan) ->
    CreatedAt = {datetime, calendar:local_time()},
    case emysql:insert(mit_dslans, [{created_at, CreatedAt}|Dslan]) of
    {updated, {1, Id}} ->
        {value, Ip} = dataset:get_value(ip, Dslan),
        mit:update(#entry{dn = to_binary(Dn), uid = mit_util:uid(dslan, Id), ip=Ip, type = dslan, data = [{id, Id}|Dslan]});
    {updated, {0, _}} ->
        ?WARNING("cannot find inserted dslan: ~p ~p", [Dn, Dslan]);
    {error, Reason} ->
        ?ERROR("dn :~p, Reason: ~p", [Dn, Reason]);
    _ ->
        ok
    end.

update_dslan(Dn, OldAttrs, Attrs) ->
     ?INFO("update dslan: ~p，~p", [Dn,Attrs]),
      case mit_util:merge(Attrs, OldAttrs) of
        {changed, MergedAttrs,_} ->
            {value, Id} = dataset:get_value(id, OldAttrs, -1),
            {value, Ip} = dataset:get_value(ip, MergedAttrs),
            MergedAttrs1 = lists:keydelete(id, 1, MergedAttrs),
            Datetime = {datetime, calendar:local_time()},
            case emysql:update(mit_dslans, [{updated_at, Datetime} | MergedAttrs1], {id, Id}) of
            {updated, {1, _}} -> %update mit cache
                mit:update(#entry{dn = to_binary(Dn), ip=Ip,type = dslan,uid=mit_util:uid(dslan, Id),
                    parent = mit_util:bdn(Dn), data = MergedAttrs});
            {updated, {0, _Id}} ->
                ?WARNING("stale Dslan: ~p", [Dn]);
            {error, Reason} ->
                ?ERROR("~p", [Reason])
            end;
        {unchanged, _,_} ->
            ok
    end.


transform(Attrs) ->
    transform(Attrs, []).

transform([{vendor, Vendor}|T], Acc) ->
    ManuId = mit_dict:lookup(vendor, Vendor),
    transform(T, [{device_manu, ManuId}|Acc]);
transform([{type, Type}|T], Acc) ->
	TypeId = mit_dict:lookup(type, Type),
	transform(T, [{device_kind, TypeId},{dslan_type,Type}|Acc]);
transform([H|T], Acc) ->
    transform(T, [H | Acc]);

transform([], Acc) ->
    Acc.


