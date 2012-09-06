-module(mit_olt).

-create("hejin 2012-8-8").

-include("mit.hrl").
-include_lib("elog/include/elog.hrl").

-import(extbif, [to_binary/1, to_list/1]).

-mit_boot_load({olt, load, "loading olt", undefined}).

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
    Sql = "select t2.means as means, t1.*,'olt' device_type   from mit_olts t1 LEFT join collect_means t2 on
        (t1.cityid = t2.cityid and t1.device_manu = t2.device_manu) where t2.means is not null",
    get_data(Sql).

one(Id) ->
    Sql = "select t2.means as means, t1.* ,'olt' device_type  from mit_olts t1 LEFT join collect_means t2 on
        (t1.cityid = t2.cityid and t1.device_manu = t2.device_manu) where t2.means is not null and t1.id = " ++ to_list(Id),
    get_data(Sql).

redisco() ->
    Sql = "select t2.means as means, t1.* ,'olt' device_type  from mit_olts t1 LEFT join collect_means t2 on
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
     cityid,
     branch_id,
     sysoid,
	 olt_pon_type,
     ip,
     mask,
     mac,
	 ping_status,
	 snmp_status,
     device_manu,
     device_kind,
     is_discovery,
     discovery_state,
     olt_state,
     snmp_r,
     snmp_w,
     snmp_v
    ].


get_entry(Olt) ->
    mit_util:format(mit, mem_attrs(), Olt).

get_notify_entry(Olt) ->
    {value, Ip} = dataset:get_value(ip, Olt),
	Dn = "olt=" ++ to_list(Ip),
    Attrs = mit_util:format(notify, attrs(), Olt),
     [{dn, list_to_binary(Dn)}|Attrs].


lookup(Dn) ->
    case mit:lookup(Dn) of
    {ok, #entry{data = Olt}} ->
        {ok, Olt};
    false ->
        false
    end.

load() ->
    case emysql:select({mit_olts, mem_attrs()}) of
        {ok, Olts} ->
            lists:foreach(fun(Olt) ->
                {value, Id} = dataset:get_value(id, Olt),
                {value, Ip} = dataset:get_value(ip, Olt),
                Dn = "olt=" ++ to_list(Ip),
                Entry = #entry{dn = to_binary(Dn), uid = mit_util:uid(olt,Id),ip=Ip, type = olt, parent = undefined, data = Olt},
                mit:update(Entry)
            end, Olts),
            io:format("finish start olt : ~p ~n", [length(Olts)]),
            {ok, state};
        {error, Reason} ->
            ?ERROR("start failure...~p",[Reason]),
            {stop, Reason}
    end.

add(Dn, Attrs) ->
    Olt = transform(Attrs),
    case mit:lookup(Dn) of
        {ok, #entry{data = Entry} = _} ->
            update_olt(Dn, Entry, Olt);
        false ->
            insert_olt(Dn, Olt)
    end.

update(Dn, Attrs) ->
    case mit:lookup(Dn) of
    {ok, #entry{data = Entry} = _} ->
        update_olt(Dn, Entry, Attrs);
    false ->
        ?ERROR("cannot find olt ~p", [Dn])
    end.

insert_olt(Dn, Olt) ->
    CreatedAt = {datetime, calendar:local_time()},
    case emysql:insert(mit_olts, [{created_at, CreatedAt}|Olt]) of
    {updated, {1, Id}} ->
        {value, Ip} = dataset:get_value(ip, Olt),
        mit:update(#entry{dn = to_binary(Dn), uid = mit_util:uid(olt, Id), ip=Ip, type = olt, data = [{id, Id}|Olt]});
    {updated, {0, _}} ->
        ?WARNING("cannot find inserted olt: ~p ~p", [Dn, Olt]);
    {error, Reason} ->
        ?ERROR("dn :~p, Reason: ~p", [Dn, Reason]);
    _ ->
        ok
    end.

update_olt(Dn, OldAttrs, Attrs) ->
   %   ?INFO("update olt: ~p，~p", [Dn,Attrs]),
      case mit_util:merge(Attrs, OldAttrs) of
        {changed, MergedAttrs,_} ->
            {value, Id} = dataset:get_value(id, OldAttrs, -1),
            {value, Ip} = dataset:get_value(ip, MergedAttrs),
            MergedAttrs1 = lists:keydelete(id, 1, MergedAttrs),
            Datetime = {datetime, calendar:local_time()},
            case emysql:update(mit_olts, [{updated_at, Datetime} | MergedAttrs1], {id, Id}) of
            {updated, {1, _}} -> %update mit cache
                mit:update(#entry{dn = to_binary(Dn), ip=Ip,type = olt,uid=mit_util:uid(olt, Id),
                    parent = mit_util:bdn(Dn), data = MergedAttrs});
            {updated, {0, _Id}} ->
                ?WARNING("stale Olt: ~p", [Dn]);
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
	transform(T, [{device_kind, TypeId},{olt_type,Type}|Acc]);
transform([H|T], Acc) ->
    transform(T, [H | Acc]);

transform([], Acc) ->
    Acc.


