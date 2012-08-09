-module(mit_vlan).

-author("hejin-2011-2-24").

-include("mit.hrl").
-include_lib("elog/include/elog.hrl").


%api
-export([lookup/1,
		 add/2]).

-import(extbif, [to_list/1, to_binary/1]).


lookup(Dn) ->
    emysql:select({mit_vlans, {vlan_dn, Dn}}).

add(Dn, Vlan) ->
    case lookup(to_binary(Dn)) of
        {ok, []} ->
            insert_vlan(Dn, Vlan);
        {ok, [OldData]} ->
            update_vlan(Dn, OldData, Vlan);
        {error, _} ->
            ?WARNING("select vlan error:~p",[Dn])
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
        case emysql:insert(mit_vlans, [{created_at, DateTime}, {vlan_dn, Dn},{olt_id, OltId},{gem_id, GemId} | Vlan]) of
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
        {ok, [Gem]} ->
            {value, Id} = dataset:get_value(id, Gem, -1),
            Id;
        _ ->
             ?WARNING("cannot find gem_id:~p",[GemDn]), 0
     end.
