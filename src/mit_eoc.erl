
-module(mit_eoc).

-author('chibj.opengoss@gmail.com').

-include("mit.hrl").
-include_lib("elog/include/elog.hrl").

-import(extbif, [to_binary/1, to_list/1]).

-mit_boot_load({eoc, load, "loading cpe", undefined}).

-export([load/0,
        all/0,
         one/1,
         redisco/0
         ]).

-export([attrs/0,
         lookup/1,
         get_entry/1,
         get_notify_entry/1,
         add/2,
         update/2]).

-define(SERVER, ?MODULE).

all() ->
    Sql = "select  'snmp' means, 'eoc' device_type,t1.* from mit_eoc_heads t1" ,
    get_data(Sql).

one(Id) ->
    Sql = "select  'snmp' means, 'eoc' device_type, t1.* from mit_eoc_heads t1 where  t1.id = " ++ to_list(Id),
    get_data(Sql).

redisco() ->
    Sql = "select  'snmp' means, 'eoc' device_type, t1.* from mit_eoc_heads t1 where  t1.discovery_state = 2",
    get_data(Sql).

get_data(Sql) ->
    case emysql:sqlquery(Sql) of
        {ok, Records} ->
		    ?INFO("eoc ~p ", [Records]),
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
     sysoid,
     name,
     ip,
     mask,
     mac,
     device_manu,
     device_kind,
     is_discovery,
     discovery_state,
	 collect_status,
     head_status,
     snmp_r,
     snmp_w,
     snmp_v,
	 oper_mode,
	 radio_channe,
	 software_version,
	 hardware_version,
	 cpe_num
    ].


get_entry(Eoc) ->
    mit_util:format(mit, mem_attrs(), Eoc).

get_notify_entry(Eoc) ->
    {value, Ip} = dataset:get_value(ip, Eoc),
	Dn = "eoc=" ++ to_list(Ip),
    Attrs = mit_util:format(notify, attrs(), Eoc),
     [{dn, list_to_binary(Dn)}|Attrs].


lookup(Dn) ->
    case mit:lookup(Dn) of
    {ok, #entry{data = Eoc}} ->
        {ok, Eoc};
    false ->
        false
    end.

load() ->
    case emysql:select({mit_eoc_heads, mem_attrs()}) of
        {ok, Eocs} ->
            lists:foreach(fun(Eoc) ->
	              io:format("I want look at Eoc: ~p ~n", [Eoc]),
               {value, Id} = dataset:get_value(id, Eoc),
                {value, Ip} = dataset:get_value(ip, Eoc),
                Dn = "eoc=" ++ to_list(Ip),
                Entry = #entry{dn = to_binary(Dn), uid = mit_util:uid(eoc,Id), type = eoc, data = Eoc},
                mit:update(Entry)
            end, Eocs),
            io:format("finish start eocs header : ~p ~n", [length(Eocs)]),
            {ok, state};
        {error, Reason} ->
            ?ERROR("start eocs failure...~p",[Reason]),
            {ok, state}
    end.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
add(Dn, Attrs) ->
    Eoc = transform(Attrs),
    case mit:lookup(Dn) of
        {ok, #entry{data = Entry} = _} ->
            update_eoc(Dn, Entry, Eoc);
        false ->
            insert_eoc(Dn, Eoc)
    end.

update(Dn, Attrs) ->
    case mit:lookup(Dn) of
    {ok, #entry{data = Entry} = _} ->
        update_eoc(Dn, Entry, Attrs);
    false ->
        ?ERROR("cannot find eoc ~p", [Dn])
    end.
    
insert_eoc(Dn, Eoc) ->
    CreatedAt = {datetime, calendar:local_time()},
    case emysql:insert(mit_eoc_heads, [{created_at, CreatedAt}|Eoc]) of
    {updated, {1, Id}} ->
        mit:update(#entry{dn = to_binary(Dn), uid = mit_util:uid(eoc, Id), type = eoc, data = [{id, Id}|Eoc]});
    {updated, {0, _}} ->
        ?WARNING("cannot find inserted eoc: ~p ~p", [Dn, Eoc]);
    {error, Reason} ->
        ?ERROR("dn :~p, Reason: ~p", [Dn, Reason]);
    _ ->
        ok
    end.

update_eoc(Dn, OldAttrs, Attrs) ->
     ?INFO("update eoc: ~p，~p", [Dn,Attrs]),
      case mit_util:merge(Attrs, OldAttrs) of
        {changed, MergedAttrs} ->
            {value, Id} = dataset:get_value(id, OldAttrs, -1),
            MergedAttrs1 = lists:keydelete(id, 1, MergedAttrs),
            Datetime = {datetime, calendar:local_time()},
            case emysql:update(mit_eoc_heads, [{updated_at, Datetime} | MergedAttrs1], {id, Id}) of
            {updated, {1, _Id}} -> %update mit cache
                mit:update(#entry{dn = Dn, uid = mit_util:uid(eoc,Id), type = eoc, data = MergedAttrs});
            {updated, {0, _Id}} ->
                ?WARNING("stale eoc: ~p", [Dn]);
            {error, Reason} ->
                ?ERROR("error ~p", [Reason])
            end;
        {unchanged, _} ->
            ok
    end.


transform(Attrs) ->
    transform(Attrs, []).

transform([{vendor, Vendor}|T], Acc) ->
    ManuId = mit_dict:lookup(vendor, Vendor),
    transform(T, [{device_manu, ManuId}|Acc]);
transform([{type, Type}|T], Acc) ->
	TypeId = mit_dict:lookup(type, Type),
	transform(T, [{device_kind, TypeId},{eoc_type,Type}|Acc]);
transform([H|T], Acc) ->
    transform(T, [H | Acc]);

transform([], Acc) ->
    Acc.

