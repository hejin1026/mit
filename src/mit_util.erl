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
    uid(clt, Id);
uid(4, Id) ->
    uid(cnu, Id);
uid(5, Id) ->
    uid(dslam, Id);
uid(clt, Id) ->
    to_binary("clt:" ++ integer_to_list(Id));
uid(cnu, Id) ->
    to_binary("cnu:" ++ integer_to_list(Id));
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
    to_binary("vlan:" ++ integer_to_list(Id));
uid(dslam, Id) ->
    to_binary("dslam:" ++ integer_to_list(Id)).

nid(undefined) ->
    undefined;
nid(Uid) ->
    [_Type,Id] = string:tokens(to_list(Uid), ":"),
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
mit_entry(clt, Clt) ->
    mit_clt:get_entry(Clt);
mit_entry(cnu, Cnu) ->
    mit_cnu:get_entry(Cnu);
mit_entry(dslam, Dslam) ->
    mit_dslam:get_entry(Dslam).

notify_entry(olt, Olt) ->
    mit_olt:get_notify_entry(Olt);
notify_entry(port, Port) ->
    mit_port:get_notify_entry(Port);
notify_entry(onu, Onu) ->
    mit_onu:get_notify_entry(Onu);
notify_entry(clt, Clt) ->
    mit_clt:get_notify_entry(Clt);
notify_entry(cnu, Cnu) ->
    mit_cnu:get_notify_entry(Cnu);
notify_entry(dslam, Dslam) ->
    mit_dslam:get_notify_entry(Dslam).


get_type(olt) -> ?OLT;
get_type(onu) -> ?ONU;
get_type(clt) -> ?CLT;
get_type(cnu) -> ?CNU;
get_type(dslam) -> ?DSLAM;

get_type(?OLT) -> olt;
get_type(?ONU) -> onu;
get_type(?CLT) -> clt;
get_type(?CNU) -> cnu;
get_type(?DSLAM) -> dslam.


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
    Attrs0 = Attrs -- OldAttrs,
    case mit_util:merge(Attrs0, OldAttrs) of
        {changed, MergedAttrs,_} ->
            Uid = proplists:get_value(id, OldAttrs,"-1"),
            ?INFO("change attr: ~p,~p", [Uid,MergedAttrs--OldAttrs]),
            case emysql:update(Table, [{id,Uid},{updated_at, {datetime, calendar:local_time()}} | MergedAttrs--OldAttrs]) of
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