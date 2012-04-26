-module(mit_util).

-created("hejin 2012-4-26").

-include_lib("elog/include/elog.hrl").
-include("mit.hrl").

-import(extbif, [to_list/1, to_binary/1]).

%utility functions
-export([bdn/1,
         rdn/1,
		 uid/2,
         get_type/1,
         format/3,
         merge/2,
         get_entry/2,
         notify_entry/2,
         mit_entry/2
         ]).


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
    to_binary("olt:" ++ integer_to_list(Id));
uid(2, Id) ->
    to_binary("onu:" ++ integer_to_list(Id));
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
    to_binary("vlan:" ++ integer_to_list(Id)).


merge(NewAttrs, OldAttrs) ->
    merge(unchanged, NewAttrs, OldAttrs).

merge(Changed, [{Attr, Val}|T], OldAttrs) ->
    case dataset:get_value(Attr, OldAttrs) of
    {value, OldVal} ->
        case to_list(OldVal) == to_list(Val) of
            false ->
                ?INFO("mit changed, ~p",[{OldVal, Val}]),
                OldAttrs1 = dataset:key_replace(Attr, OldAttrs, {Attr, Val}),
                merge(changed, T, OldAttrs1);
            true ->
                merge(Changed, T, OldAttrs)
        end;
    {false, _} ->
        merge(changed, T, [{Attr, Val} | OldAttrs])
    end;

merge(Changed, [], OldAttrs) ->
    {Changed, OldAttrs}.


get_entry(olt, DevId) ->
    mit_olt:one(DevId);
get_entry(onu, DevId) ->
    mit_onu:one(DevId).

mit_entry(olt, Olt) ->
    mit_olt:get_entry(Olt);
mit_entry(onu, Olt) ->
    mit_onu:get_entry(Olt).

notify_entry(olt, Olt) ->
    mit_olt:get_notify_entry(Olt);
notify_entry(port, Port) ->
    mit_port:get_notify_entry(Port);
notify_entry(onu, Onu) ->
    mit_onu:get_notify_entry(Onu).



get_type(olt) -> ?OLT;
get_type(?OLT) -> olt;
get_type(?ONU) -> onu;
get_type(onu) -> ?ONU.



format(Type, Attrs, Entry) ->
    format(Type, Attrs, Entry, []).

format(_, [], _Entry, Data) ->
    lists:reverse(Data);
format(notify, [device_type|Attrs], Entry, Data) ->
	{value, Device_type} = dataset:get_value(device_type, Entry),
    format(notify, Attrs, Entry, [{device_type, extbif:to_atom(Device_type)}|Data]);
format(notify, [device_kind|Attrs], Entry, Data) ->
	{value, TypeId} = dataset:get_value(device_kind, Entry),
    Type =  mit_dict:lookup(type, TypeId),
    format(notify, Attrs, Entry, [{type, Type}, {device_kind, TypeId}|Data]);
format(notify, [olt_state|Attrs], Entry, Data) ->
    {value, Value} = dataset:get_value(olt_state, Entry),
    format(notify, Attrs, Entry, [{oper_state, Value}|Data]);
format(notify, [onu_state|Attrs], Entry, Data) ->
    {value, Value} = dataset:get_value(onu_state, Entry),
    format(notify, Attrs, Entry, [{oper_state, Value}|Data]);
format(notify, [device_manu|Attrs], Entry, Data) ->
    {value, VendorId} = dataset:get_value(device_manu, Entry),
    Vendor =  mit_dict:lookup(vendor, VendorId),
    format(notify, Attrs, Entry, [{vendor, Vendor}, {device_manu, VendorId}|Data]);
format(Type, [Item|Attrs], Entry, Data) ->
	case dataset:get_value(Item, Entry, false) of
		{value, <<"false">>} ->
			format(Type, Attrs, Entry, Data);
		{value, Value} ->
			format(Type, Attrs, Entry, [{Item, Value}|Data])
		end.

