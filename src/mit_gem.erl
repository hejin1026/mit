-module(mit_gem).

-author('hejin-2-18').

-include("mit.hrl").
-include_lib("elog/include/elog.hrl").

%api
-export([attrs/0,
         lookup/1,
		 add/2,
		 update/2]).

-import(extbif, [to_list/1, to_binary/1]).

get_uid(Id) ->
    list_to_binary("onu:" ++ integer_to_list(Id)).

lookup(Dn) ->
    emysql:select({mit_gems, {gem_dn, Dn}}).


add(GemDn, Gem) ->
    case lookup(GemDn) of
        {ok, OldGem} ->
            update_gem(GemDn, OldGem, Gem);
        {error, _} ->
            insert_gem(GemDn, Gem)
    end.

update(GemDn, Attrs) ->
    case lookup(GemDn) of
        {ok, OldAttrs} ->
            update_gem(GemDn, OldAttrs, Attrs);
        {error, _} ->
            ?ERROR("cannot find gem ~p", [GemDn])
    end.


update_gem(Dn, OldAttrs, Attrs) ->
    {value, SlotNo} = dataset:get_value(slot_no, Attrs),
    {value, PortNo} = dataset:get_value(port_no, Attrs),
    {value, OnuNo} = dataset:get_value(onu_no, Attrs),
    OltDn = lists:last(string:tokens(to_list(Dn), ",")),
    NewAttrs = case get_onu_id(OltDn, SlotNo, PortNo, OnuNo) of
        0 -> Attrs;
        Id -> [{onu_id, Id}|Attrs]
    end,
    ?INFO("update gem, oldattr: ~p, newattr: ~p", [OldAttrs, NewAttrs]),
    case mit_util:merge(NewAttrs, OldAttrs) of
        {changed, MergedAttrs} ->
            ?INFO("update gem, mergeattr: ~p", [MergedAttrs]),
            {value, GemId} = dataset:get_value(id, OldAttrs, -1),
            LastChanged = {datetime, calendar:local_time()},
            MergedAttrs2 = lists:keydelete(id, 1, MergedAttrs),
            case emysql:update(mit_gems, [{updated_at, LastChanged} | MergedAttrs2], {id, GemId}) of
                {updated, _} -> 
                    ok;
                {error, Reason} ->
                    ?ERROR("~p", [Reason])
                end;
        {unchanged, _} ->
            ok
    end.

insert_gem(Dn, Gem) ->
    OltDn = lists:last(string:tokens(to_list(Dn), ",")),
    {value, SlotNo} = dataset:get_value(slot_no, Gem),
    {value, PortNo} = dataset:get_value(port_no, Gem),
    {value, OnuNo} = dataset:get_value(onu_no, Gem),
    case mit:lookup(to_binary(OltDn)) of
    {ok, #entry{data = Olt}} ->
        ?INFO("insert gem: ~p", [Dn]),
        {value, OltId} = dataset:get_value(id, Olt),
        OnuId = get_onu_id(OltDn, SlotNo, PortNo, OnuNo),
        DateTime = {datetime, calendar:local_time()},
        case emysql:insert(mit_gems, [{created_at, DateTime}, {updated_at, DateTime}, {olt_id, OltId}, {onu_id, OnuId} | Gem]) of
            {updated, _} ->
                ok;
            {error, Reason} ->
                ?WARNING("~p", [Reason])
            end;
    false ->
        ?WARNING("cannot find olt: ~p", [OltDn])
    end.


get_onu_id(OltDn, SlotNo, PortNo, OnuNo) ->
     Entries = mnesia:dirty_match_object({entry, '_', '_', to_binary(OltDn), onu, '_'}),
     ?INFO("onus:~p, ~p, ~n,~p",[[OltDn,SlotNo, PortNo, OnuNo], length(Entries), Entries]),
     lists:foldl(fun(Onu, Id) ->
        OnuInfo = Onu#entry.data,
        "onu:"++OnuId = Onu#entry.uid,
        {value, SlotNo2} = dataset:get_value(slot_no, OnuInfo),
        {value, PortNo2} = dataset:get_value(port_no, OnuInfo),
        {value, OnuNo2} = dataset:get_value(onu_no, OnuInfo),
        case {SlotNo2, PortNo2, OnuNo2} of
            {SlotNo, PortNo, OnuNo} -> %1 ->olt 下的onu
                ?INFO("get gem_id by ~p",[{SlotNo, PortNo, OnuNo}]),
                list_to_integer(OnuId);
             _ ->
                Id
        end
     end, 0, Entries).