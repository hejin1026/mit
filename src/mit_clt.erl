
-module(mit_clt).

-author('chibj.opengoss@gmail.com').

-include("mit.hrl").
-include_lib("elog/include/elog.hrl").

-import(extbif, [to_binary/1, to_list/1]).

-mit_boot_load({clt, load, "loading cnu", undefined}).

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
    Sql = "select  'snmp' means, 'clt' device_type,t1.* from mit_clts t1" ,
    get_data(Sql).

one(Id) ->
    Sql = "select  'snmp' means, 'clt' device_type, t1.* from mit_clts t1 where  t1.id = " ++ to_list(Id),
    get_data(Sql).

redisco() ->
    Sql = "select  'snmp' means, 'clt' device_type, t1.* from mit_clts t1 where  t1.discovery_state = 2",
    get_data(Sql).

get_data(Sql) ->
    case emysql:sqlquery(Sql) of
        {ok, Records} ->
		    ?INFO("clt ~p ", [Records]),
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
	 cnu_num
    ].


get_entry(Clt) ->
    mit_util:format(mit, mem_attrs(), Clt).

get_notify_entry(Clt) ->
    {value, Ip} = dataset:get_value(ip, Clt),
	Dn = "clt=" ++ to_list(Ip),
    Attrs = mit_util:format(notify, attrs(), Clt),
     [{dn, list_to_binary(Dn)}|Attrs].


lookup(Dn) ->
    case mit:lookup(Dn) of
    {ok, #entry{data = Clt}} ->
        {ok, Clt};
    false ->
        false
    end.

load() ->
    case emysql:select({mit_clts, mem_attrs()}) of
        {ok, Clts} ->
            lists:foreach(fun(Clt) ->
	            %  io:format("I want look at Clt: ~p ~n", [Clt]),
               {value, Id} = dataset:get_value(id, Clt),
                {value, Ip} = dataset:get_value(ip, Clt),
                Dn = "clt=" ++ to_list(Ip),
                Entry = #entry{dn = to_binary(Dn), uid = mit_util:uid(clt,Id), ip= Ip,type = clt, data = Clt},
                mit:update(Entry)
            end, Clts),
            io:format("finish start clts header : ~p ~n", [length(Clts)]),
            {ok, state};
        {error, Reason} ->
            ?ERROR("start clts failure...~p",[Reason]),
            {ok, state}
    end.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
add(Dn, Attrs) ->
    Clt = transform(Attrs),
    case mit:lookup(Dn) of
        {ok, #entry{data = Entry} = _} ->
            update_clt(Dn, Entry, Clt);
        false ->
            insert_clt(Dn, Clt)
    end.

update(Dn, Attrs) ->
    case mit:lookup(Dn) of
    {ok, #entry{data = Entry} = _} ->
        update_clt(Dn, Entry, Attrs);
    false ->
        ?ERROR("cannot find clt ~p", [Dn])
    end.

insert_clt(Dn, Clt) ->
    CreatedAt = {datetime, calendar:local_time()},
    case emysql:insert(mit_clts, [{created_at, CreatedAt}|Clt]) of
    {updated, {1, Id}} ->
        mit:update(#entry{dn = to_binary(Dn), uid = mit_util:uid(clt, Id), type = clt, data = [{id, Id}|Clt]});
    {updated, {0, _}} ->
        ?WARNING("cannot find inserted clt: ~p ~p", [Dn, Clt]);
    {error, Reason} ->
        ?ERROR("dn :~p, Reason: ~p", [Dn, Reason]);
    _ ->
        ok
    end.

update_clt(Dn, OldAttrs, Attrs) ->
     ?INFO("update clt: ~p，~p", [Dn,Attrs]),
      case mit_util:merge(Attrs, OldAttrs) of
        {changed, MergedAttrs,_} ->
            {value, Id} = dataset:get_value(id, OldAttrs, -1),
            MergedAttrs1 = lists:keydelete(id, 1, MergedAttrs),
            Datetime = {datetime, calendar:local_time()},
            case emysql:update(mit_clts, [{updated_at, Datetime} | MergedAttrs1], {id, Id}) of
            {updated, {1, _Id}} -> %update mit cache
                mit:update(#entry{dn = Dn, uid = mit_util:uid(clt,Id), type = clt, data = MergedAttrs});
            {updated, {0, _Id}} ->
                ?WARNING("stale clt: ~p", [Dn]);
            {error, Reason} ->
                ?ERROR("error ~p", [Reason])
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
	transform(T, [{device_kind, TypeId},{clt_type,Type}|Acc]);
transform([H|T], Acc) ->
    transform(T, [H | Acc]);

transform([], Acc) ->
    Acc.

