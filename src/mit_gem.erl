-module(mit_gem).

-author('hejin-2-18').

-include("mit.hrl").
-include_lib("elog/include/elog.hrl").

-behavior(gen_server).

-export([start_link/0,
         stop/0]).

%api
-export([attrs/0,
         lookup/1,
		 add/2,
		 update/2]).

-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3]).

-import(extbif, [to_list/1, to_binary/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, stop).

attrs() ->
    [
        id,
        line_profile,
        gem_no,
        gem_type,
        tcont,
        dba_type,
        olt_id,
        onu_id,
        slot_no,
        port_no,
        onu_no,
        downassuredbw,
        downmaximumbw,
        downfixedbw,
        upassuredbw,
        upmaximumbw,
        upfixedbw
    ].

get_uid(Id) ->
    list_to_binary("onu:" ++ integer_to_list(Id)).

lookup(Dn) ->
    case mit:lookup(Dn) of
    {ok, #entry{data = Gem, type = gem}} ->
        {ok, Gem};
    false ->
        false;
    _Other ->
        false
    end.

add(Dn, Gem) ->
	gen_server:cast(?MODULE, {add, Dn, Gem}).

update(Dn, Attrs) ->
    gen_server:cast(?MODULE, {update, Dn, Attrs}).

init([]) ->
    case emysql:select(mit_gems, attrs()) of
    {ok, Gems} ->
        lists:foreach(fun(Gem) ->
            {value, GemNo} = dataset:get_value(gem_no, Gem),
            {value, SlotNo} = dataset:get_value(slot_no, Gem),
            {value, PortNo} = dataset:get_value(port_no, Gem),
            {value, OnuNo} = dataset:get_value(onu_no, Gem),
            {value, OltId} = dataset:get_value(olt_id, Gem),
            OltUid = mit_util:uid(olt, OltId),
            case mit:lookup(id, to_binary(OltUid)) of
            {ok, #entry{dn = OltDn}} ->
                {value, GemId} = dataset:get_value(id, Gem),
                GemDn = lists:concat(["gem=", GemNo, ",", 
                                       "onu=", OnuNo, ",",
                                       "port=", PortNo, ",",
                                       "slot=", SlotNo, ",",
                                       to_list(OltDn)]),
                mit:update(#entry{dn = to_binary(GemDn), uid = mit_util:uid(gem, GemId), type = gem, parent = mit_util:bdn(GemDn), data = Gem});
            false ->
                io:format("cannot find olt: ~p", [OltId])
            end
        end, Gems),
        ?PRINT(" cached ~p Gems ", [length(Gems)]),
        {ok, state};
    {error, Reason} ->
        {stop, Reason}
    end.

handle_call(stop, _From, State) ->
	{stop, normal, ok, State};

handle_call(Request, _From, State) ->
	?ERROR("unexpected requrest: ~n", [Request]),
    {reply, {error, unexpected_request}, State}.




handle_cast({add, GemDn, Gem}, State) ->
    case lookup(GemDn) of
    {ok, OldGem} ->
        update_gem(GemDn, OldGem, Gem);
    false ->
        insert_gem(GemDn, Gem)
    end,
    {noreply, State};

handle_cast({update, GemDn, Attrs}, State) ->
    case lookup(GemDn) of
    {ok, OldAttrs} ->
        update_gem(GemDn, OldAttrs, Attrs);
    false ->
        ?ERROR("cannot find gem ~p", [GemDn])
    end,
    {noreply, State};

handle_cast(Msg, State) ->
	?ERROR("unexpected msg: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    ?ERROR("unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



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
                {updated, {1, _Id}} -> %update mit cache
                    mit:update(#entry{dn = Dn, uid = mit_util:uid(gem, GemId), type = gem, parent = mit_util:bdn(Dn), data = MergedAttrs});
                {updated, {0, _Id}} -> %stale port?
                    ?WARNING("stale port: ~p", [Dn]);
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
            {updated, {1, Id}} ->
                mit:update(#entry{dn = to_binary(Dn), uid = mit_util:uid(gem, Id), type = gem, parent = mit_util:bdn(Dn),
                    data = [{id, Id}|Gem]});
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