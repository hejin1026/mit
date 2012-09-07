-module(mit_util).

-created("hejin 2012-4-26").

-include_lib("elog/include/elog.hrl").
-include("mit.hrl").

-import(extbif, [to_list/1, to_binary/1]).

%utility functions
-export([bdn/1,
         rdn/1,
		 uid/2,
         nid/1,
         get_type/1,
         get_key/1,
         format/3,
         merge/2,
         get_entry/2,
         notify_entry/2,
         mit_entry/2
         ]).

-export([do_update/4]).

bdn(Dn) when is_binary(Dn) ->
    bdn(binary_to_list(Dn));

bdn([]) ->
    [];

bdn(Dn) when is_list(Dn) ->
	[_|T] = string:tokens(Dn, ","),
	list_to_binary(string:join(T, ",")).

rdn(Dn) when is_binary(Dn) ->
    rdn(binary_to_list(Dn));

rdn(Dn) when is_list(Dn) ->
    [Rdn|_] = string:tokens(Dn, ","),
    list_to_binary(Rdn).

uid(1, Id) ->
    uid(olt, Id);
uid(2, Id) ->
    uid(onu, Id);
uid(3, Id) ->
    uid(eoc, Id);
uid(4, Id) ->
    uid(cpe, Id);
uid(5, Id) ->
    uid(dslan, Id);
uid(eoc, Id) ->
    to_binary("eoc:" ++ integer_to_list(Id));
uid(cpe, Id) ->
    to_binary("cpe:" ++ integer_to_list(Id));
uid(olt, Id) ->
    to_binary("olt:" ++ integer_to_list(Id));
uid(onu, Id) ->
    to_binary("onu:" ++ integer_to_list(Id));
uid(port, Id) ->
    to_binary("port:" ++ integer_to_list(Id));
uid(splite, Id) ->
    to_binary("splite:" ++ integer_to_list(Id));
uid(gem, Id) ->
    to_binary("gem:" ++ integer_to_list(Id));
uid(vlan, Id) ->
    to_binary("vlan:" ++ integer_to_list(Id)),
uid(dslan, Id) ->
    to_binary("dslan:" ++ integer_to_list(Id)).

nid(undefined) ->
    undefined;
nid(Uid) ->
    [_Type,Id] = string:tokens(binary_to_list(Uid), ":"),
    list_to_integer(Id).

merge(NewAttrs, OldAttrs) ->
    merge(unchanged, NewAttrs, OldAttrs, []).

merge(Changed, [{Attr, Val}|T], OldAttrs, Items) ->
    case dataset:get_value(Attr, OldAttrs) of
    {value, OldVal} ->
        case to_list(OldVal) == to_list(Val) of
            false ->
                ?INFO("mit changed, ~p",[{OldVal, Val}]),
                OldAttrs1 = dataset:key_replace(Attr, OldAttrs, {Attr, Val}),
                merge(changed, T, OldAttrs1, [Attr|Items]);
            true ->
                merge(Changed, T, OldAttrs, Items)
        end;
    {false, _} ->
        merge(changed, T, [{Attr, Val} | OldAttrs], [Attr|Items])
    end;

merge(Changed, [], OldAttrs, Items) ->
    {Changed, OldAttrs, Items}.


get_entry(olt, DevId) ->
    mit_olt:one(DevId);
get_entry(onu, DevId) ->
    mit_onu:one(DevId).

mit_entry(olt, Olt) ->
    mit_olt:get_entry(Olt);
mit_entry(onu, Onu) ->
    mit_onu:get_entry(Onu);
mit_entry(eoc, Eoc) ->
    mit_eoc:get_entry(Eoc);
mit_entry(cpe, Cpe) ->
    mit_cpe:get_entry(Cpe);
mit_entry(dslan, Dslan) ->
    mit_dslan:get_entry(Dslan).

notify_entry(olt, Olt) ->
    mit_olt:get_notify_entry(Olt);
notify_entry(port, Port) ->
    mit_port:get_notify_entry(Port);
notify_entry(onu, Onu) ->
    mit_onu:get_notify_entry(Onu);
notify_entry(eoc, Eoc) ->
    mit_eoc:get_notify_entry(Eoc);
notify_entry(cpe, Cpe) ->
    mit_cpe:get_notify_entry(Cpe);
notify_entry(dslan, Dslan) ->
    mit_dslan:get_notify_entry(Dslan).


get_type(olt) -> ?OLT;
get_type(onu) -> ?ONU;
get_type(eoc) -> ?EOC;
get_type(cpe) -> ?CPE;
get_type(dslan) -> ?DSLAN;

get_type(?OLT) -> olt;
get_type(?ONU) -> onu;
get_type(?EOC) -> eoc;
get_type(?CPE) -> cpe;
get_type(?DSLAN) -> dslan;




format(Type, Attrs, Entry) ->
    format(Type, Attrs, Entry, []).

format(_, [], _Entry, Data) ->
    lists:reverse(Data);
format(notify, [device_type|Attrs], Entry, Data) ->
	{value, Device_type} = dataset:get_value(device_type, Entry),
    format(notify, Attrs, Entry, [{device_type, extbif:to_atom(Device_type)}|Data]);
format(notify, [device_kind|Attrs], Entry, Data) ->
	case dataset:get_value(device_kind, Entry) of
        {value, TypeId} ->
            Type =  mit_dict:lookup(type, TypeId),
            format(notify, Attrs, Entry, [{type, Type}, {device_kind, TypeId}|Data]);
        {false, _} ->
            format(notify, Attrs, Entry, Data)
     end;


format(notify, [collect_status|Attrs], Entry, Data) ->
    {value, Value} = dataset:get_value(collect_status, Entry,1),
    format(notify, Attrs, Entry, [{collect_status, Value}|Data]);
format(notify, [olt_state|Attrs], Entry, Data) ->
    {value, Value} = dataset:get_value(olt_state, Entry),
    format(notify, Attrs, Entry, [{collect_status, Value}|Data]);
format(notify, [onu_state|Attrs], Entry, Data) ->
    {value, Value} = dataset:get_value(onu_state, Entry),
    format(notify, Attrs, Entry, [{collect_status, Value}|Data]);
format(notify, [device_manu|Attrs], Entry, Data) ->
    {value, VendorId} = dataset:get_value(device_manu, Entry),
    Vendor =  mit_dict:lookup(vendor, VendorId),
    format(notify, Attrs, Entry, [{vendor, Vendor}, {device_manu, VendorId}|Data]);
format(Type, [Item|Attrs], Entry, Data) ->
	case dataset:get_value(Item, Entry) of
		{false, _} ->
			format(Type, Attrs, Entry, Data);
		{value, Value} ->
			format(Type, Attrs, Entry, [{Item, Value}|Data])
		end.

get_key(List) ->
    [Key || {Key, _} <- List].


do_update(Table, Attrs, OldAttrs, CallFun) ->
    case mit_util:merge(Attrs, OldAttrs) of
        {changed, MergedAttrs,_} ->
            case emysql:update(Table, [{updated_at, {datetime, calendar:local_time()}} | MergedAttrs]) of
                {updated, {1, _}} ->
					{value, Id} = dataset:get_value(id, OldAttrs),
                    if is_function(CallFun) -> CallFun(Id, MergedAttrs);
                        true -> ok
                    end;
                {updated, {0, _}} ->
                    ?WARNING("stale board: ~p", [MergedAttrs]);
                {error, Reason} ->
                    ?ERROR("~p,~p", [MergedAttrs, Reason])
            end;
       {unchanged, _,_} ->
            ok
    end.