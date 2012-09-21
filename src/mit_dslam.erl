-module(mit_dslam).

-create("chibj 2012-9-7").

-include("mit.hrl").
-include_lib("elog/include/elog.hrl").

-import(extbif, [to_binary/1, to_list/1]).

-mit_boot_load({dslam, load, "loading dslam", undefined}).

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
    Sql = "select  'snmp' means, 'dslam' device_type,t1.* from mit_dslams t1 where t1.collect_status < 2" ,
    get_data(Sql).

one(Id) ->
    Sql = "select  'snmp' means, 'dslam' device_type, t1.* from mit_dslams t1 where  t1.id = " ++ to_list(Id),
    get_data(Sql).

redisco() ->
    Sql = "select  'snmp' means, 'dslam' device_type, t1.* from mit_dslams t1 where  t1.discovery_state = 2",
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


get_entry(Dslam) ->
    mit_util:format(mit, mem_attrs(), Dslam).

get_notify_entry(Dslam) ->
    {value, Ip} = dataset:get_value(ip, Dslam),
	Dn = "dslam=" ++ to_list(Ip),
    Attrs = mit_util:format(notify, attrs(), Dslam),
     [{dn, list_to_binary(Dn)}|Attrs].


lookup(Dn) ->
    case mit:lookup(Dn) of
    {ok, #entry{data = Dslam}} ->
        {ok, Dslam};
    false ->
        false
    end.

load() ->
    ?ERROR("select  dslam ...~n", []),
    case emysql:select({mit_dslams, mem_attrs()}) of
        {ok, Dslams} ->
            ?ERROR("start mem dslam ~n", []),
            Store = fun(Dslam) -> mnesia:write(entry(Dslam)) end,
            mnesia:sync_dirty(fun lists:foreach/2, [Store, Dslams]),
            io:format("finish start dslam : ~p ~n", [length(Dslams)]),
            {ok, state};
        {error, Reason} ->
            ?ERROR("start failure...~p ~n",[Reason]),
            {stop, Reason}
    end.


entry(Dsalm) ->
     {value, Id} = dataset:get_value(id, Dslam),
        {value, Ip} = dataset:get_value(ip, Dslam),
        Dn = "dslam=" ++ to_list(Ip),
        #entry{dn = to_binary(Dn), uid = mit_util:uid(dslam,Id),ip=Ip, type = dslam, parent = undefined, data = Dslam}.


add(Dn, Attrs) ->
    Dslam = transform(Attrs),
    case mit:lookup(Dn) of
        {ok, #entry{data = Entry} = _} ->
            update_dslam(Dn, Entry, Dslam);
        false ->
            insert_dslam(Dn, Dslam)
    end.

update(Dn, Attrs) ->
    case mit:lookup(Dn) of
    {ok, #entry{data = Entry} = _} ->
        update_dslam(Dn, Entry, Attrs);
    false ->
        ?ERROR("cannot find dslam ~p", [Dn])
    end.

insert_dslam(Dn, Dslam) ->
    CreatedAt = {datetime, calendar:local_time()},
    case emysql:insert(mit_dslams, [{created_at, CreatedAt}|Dslam]) of
    {updated, {1, Id}} ->
        {value, Ip} = dataset:get_value(ip, Dslam),
        mit:update(#entry{dn = to_binary(Dn), uid = mit_util:uid(dslam, Id), ip=Ip, type = dslam, data = [{id, Id}|Dslam]});
    {updated, {0, _}} ->
        ?WARNING("cannot find inserted dslam: ~p ~p", [Dn, Dslam]);
    {error, Reason} ->
        ?ERROR("dn :~p, Reason: ~p", [Dn, Reason]);
    _ ->
        ok
    end.

update_dslam(Dn, OldAttrs, Attrs) ->
     ?INFO("update dslam: ~p，~p", [Dn,Attrs]),
      case mit_util:merge(Attrs, OldAttrs) of
        {changed, MergedAttrs,_} ->
            {value, Id} = dataset:get_value(id, OldAttrs, -1),
            {value, Ip} = dataset:get_value(ip, MergedAttrs),
            MergedAttrs1 = lists:keydelete(id, 1, MergedAttrs),
            Datetime = {datetime, calendar:local_time()},
            case emysql:update(mit_dslams, [{updated_at, Datetime} | MergedAttrs1], {id, Id}) of
            {updated, {1, _}} -> %update mit cache
                mit:update(#entry{dn = to_binary(Dn), ip=Ip,type = dslam,uid=mit_util:uid(dslam, Id),
                    parent = mit_util:bdn(Dn), data = MergedAttrs});
            {updated, {0, _Id}} ->
                ?WARNING("stale Dslam: ~p", [Dn]);
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
	transform(T, [{device_kind, TypeId},{dslam_type,Type}|Acc]);
transform([H|T], Acc) ->
    transform(T, [H | Acc]);

transform([], Acc) ->
    Acc.


