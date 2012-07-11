-module(mit_vlan).

-author('hejin-2-24').

-include("mit.hrl").
-include_lib("elog/include/elog.hrl").


%api
-export([attrs/0,
         lookup/1,
		 add/2,
		 update/2]).

-import(extbif, [to_list/1, to_binary/1]).


lookup(Dn) ->
    emysql:select({mit_vlans, {vlan_dn, Dn}}).

add(Dn, Vlan) ->
    case lookup(to_binary(Dn)) of
        {ok, OldData} ->
            update_vlan(Dn, OldData, Vlan);
        {error, _} ->
            insert_vlan(Dn, Vlan)
    end.

update(Dn, Attrs) ->
    case lookup(Dn) of
        {ok, OldAttrs} ->
            update_vlan(Dn, OldAttrs, Attrs);
        {error, _} ->
            ?ERROR("cannot find vlan ~p", [Dn])
    end.

do_init() ->
    case emysql:select({mit_vlans, attrs()}) of
    {ok, Vlans} ->
        lists:foreach(fun(Vlan) ->
            {value, OltId} = dataset:get_value(olt_id, Vlan),
            {value, SlotNo} = dataset:get_value(slot_no, Vlan),
            {value, PortNo} = dataset:get_value(port_no, Vlan),
            {value, OnuNo} = dataset:get_value(onu_no, Vlan),
            {value, GemNo} = dataset:get_value(gem_no, Vlan),
            {value, VlanType} = dataset:get_value(vlan_type, Vlan),
            OltUid = mit_util:uid(olt, OltId),
            case mit:lookup(id, to_binary(OltUid)) of
            {ok, #entry{dn = OltDn}} ->
                {value, VlanId} = dataset:get_value(id, Vlan),
                VlanUid = "vlan:" ++ integer_to_list(VlanId),
                VlanDn = lists:concat(["vlan=", VlanType, ",",
                                        "gem=", GemNo, ",",
                                        "onu=", OnuNo, ",",
                                        "port=", PortNo, ",",
                                        "slot=", SlotNo, ",",
                                        to_list(OltDn)]),
                mit:update(#entry{dn = to_binary(VlanDn), uid = to_binary(VlanUid),
                    type = vlan, parent = mit_util:bdn(VlanDn), data = Vlan});
            false ->
                io:format("cannot find olt: ~p", [OltId])
            end
        end, Vlans),
        io:format("finish start vlan : ~p ~n", [length(Vlans)]),
        {ok, state};
    {error, Reason} ->
        ?ERROR("start failure...",[]),
        {stop, Reason}
    end.



update_vlan(Dn, OldAttrs, Attrs) ->
    %重新发现后可以知道gem
    GemDn = mit_util:bdn(Dn),
    NewAttrs = case get_gem_id(GemDn) of
        0 -> Attrs;
        Id -> [{gem_id, Id}|Attrs]
    end,
    ?INFO("update vlan, ~p, oldattr: ~p, newattr: ~p", [Dn, OldAttrs, NewAttrs]),
    case mit_util:merge(NewAttrs, OldAttrs) of
        {changed, MergedAttrs} ->
            {value, VlanId} = dataset:get_value(id, OldAttrs),
            LastChanged = {datetime, calendar:local_time()},
            MergedAttrs2 = lists:keydelete(id, 1, MergedAttrs),
            case emysql:update(mit_vlans, [{updated_at, LastChanged} | MergedAttrs2], {id, VlanId}) of
                {updated, _} ->
                    ok;
                {error, Reason} ->
                    ?ERROR("~p", [Reason])
                end;
        {unchanged, _} ->
            ok
    end.

insert_vlan(Dn, Vlan) ->
    OltDn = lists:last(string:tokens(to_list(Dn), ",")),
    case mit:lookup(to_binary(OltDn)) of
    {ok, #entry{data = Olt}} ->
        ?INFO("insert vlan: ~p, ~p", [Dn, Vlan]),
        {value, OltId} = dataset:get_value(id, Olt),
        {value, GemNo} = dataset:get_value(gem_no, Vlan),
        {value, SlotNo} = dataset:get_value(slot_no, Vlan),
        {value, PortNo} = dataset:get_value(port_no, Vlan),
        {value, OnuNo} = dataset:get_value(onu_no, Vlan),
        GemDn = lists:concat(["gem=", GemNo, ",",
                                "onu=", OnuNo, ",",
                                "port=", PortNo, ",",
                                "slot=", SlotNo, ",",
                                to_list(OltDn)]),
        GemId = get_gem_id(GemDn),
        DateTime = {datetime, calendar:local_time()},
        case emysql:insert(mit_vlans, [{created_at, DateTime}, {updated_at, DateTime}, {olt_id, OltId},{gem_id, GemId} | Vlan]) of
            {updated, _} ->
                ok;
            {error, Reason} ->
                ?WARNING("~p", [Reason])
            end;
    false ->
        ?WARNING("cannot find olt: ~p", [OltDn])
    end.

get_gem_id(GemDn) ->
    case mit_gem:lookup(to_binary(GemDn)) of
        {ok, Gem} ->
            {value, Id} = dataset:get_value(id, Gem, -1),
            Id;
        {error, _} ->
             ?WARNING("cannot find gem_id:~p",[GemDn]), 0
     end.
