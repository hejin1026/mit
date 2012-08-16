
-module(mit_board).

-creatd("hejin 2012-8-7").

-import(extbif, [to_binary/1,to_list/1]).

-include("mit.hrl").
-include_lib("elog/include/elog.hrl").

-mit_boot_load({board, load, "loading olt board", olt}).

%api
-export([load/0,attrs/0,
         lookup/1,
		 add/2,
		 add_boards/2,
		 update/2,
		 update_boards/2]).

-define(SERVER, ?MODULE).

attrs() ->
    [id,
     slot_no,
     device_id,
     device_type,
     operstatus,
     adminstatus,
     cpuload,
     memusage,
     standbystatus,
     lockstatus,
     boardid
    ].

lookup(Dn) ->
    case mit:lookup(Dn) of
    {ok, #entry{data = Board}} ->
		{ok, Board};
    false ->
		false
    end.

load() ->
    {ok, Boards} = emysql:sqlquery("select t.ip,o.* from mit_boards o LEFT join mit_olts t on t.id = o.device_id
        where o.device_type = 1"),
    ?ERROR("start mem board ...", []),
    lists:foreach(fun(Board) ->
        {value, Id} = dataset:get_value(id, Board),
        Buid = "slot:" ++ integer_to_list(Id),
        {value, OltIp} = dataset:get_value(ip, Board),
        OltDn = lists:concat(["olt=", to_list(OltIp)]),
        Dn = get_dn(OltIp, Board),
        mit:update(#entry{dn = to_binary(Dn), uid = to_binary(Buid),parent = OltDn,
            type = board, data = mit_util:format(mit, attrs(), Board)})
    end, Boards),
    ?ERROR("finish start board : ~p ~n", [length(Boards)]).


get_dn(OltIp, Board) ->
      {value, Boardid} = dataset:get_value(boardid, Board),
      lists:concat(["slot=", to_list(Boardid),",olt=", to_list(OltIp)]).

get_dn2(OltDn, BoardId) ->
      lists:concat(["slot=", to_list(BoardId),",", to_list(OltDn)]).

add(Dn, Attrs) ->
    case lookup(to_binary(Dn)) of
        {ok, OldAttrs} ->
            update_board(Dn, OldAttrs, Attrs);
        false ->
            insert_board(Dn, Attrs)
    end.

add_boards(Dn, Boards) ->
    case mit:lookup(Dn) of
        {ok, #entry{uid = Id, type = Type, data = Entry}} ->
            {ok, BoardInDb} = emysql:select(mit_boards,
                ({'and', {device_id, mit_util:nid(Id)}, {device_type, mit_util:get_type(Type)}})),
            List = [{to_binary(proplists:get_value(boardid, Data)), Data} || Data <- Boards],
            DbList = [{to_binary(proplists:get_value(boardid, Data)), Data} || Data <- BoardInDb],
            {AddList, UpdateList, _DelList} = extlib:list_compare(mit_util:get_key(List), mit_util:get_key(DbList)),
            [insert_board(Type, Entry, proplists:get_value(Rdn, List)) || Rdn <- AddList],
            [update_board(proplists:get_value(Rdn, DbList), proplists:get_value(Rdn, List)) || Rdn <- UpdateList];
         _ ->
             ignore
     end.

update_boards(Dn, Boards) ->
    case mit:lookup(Dn) of
        {ok, #entry{uid = Id, type = Type, data = Entry}} ->
            {ok, BoardInDb} = emysql:select(mit_boards,
                ({'and', {device_id, mit_util:nid(Id)}, {device_type, mit_util:get_type(Type)}})),
            List = [{to_binary(proplists:get_value(boardid, Data)), Data} || Data <- Boards],
            DbList = [{to_binary(proplists:get_value(boardid, Data)), Data} || Data <- BoardInDb],
            {_AddList, UpdateList, _DelList} = extlib:list_compare(mit_util:get_key(List), mit_util:get_key(DbList)),
            [update_board(proplists:get_value(Rdn, DbList), proplists:get_value(Rdn, List)) || Rdn <- UpdateList];
         _ ->
             ignore
     end.


update(Dn, Attrs) ->
    case lookup(to_binary(Dn)) of
        {ok, OldAttrs} ->
            update_board(Dn, OldAttrs, Attrs);
        false ->
            ?ERROR("cannot find board: ~p", [Dn])
    end.

insert_board(Dn, Board) ->
    case mit:lookup(mit_util:bdn(Dn)) of
    {ok, #entry{data = Entry, type = Type} = _} ->
        InsertMem = fun(Id, BoardInfo) ->
             Uid = "slot:" ++ integer_to_list(Id),
             {value, OltIp} = dataset:get_value(ip, Entry),
             mit:update(#entry{dn = get_dn(OltIp, BoardInfo), uid = Uid, type = board, parent = mit_util:bdn(Dn), data = [{id, Id}|BoardInfo]})
         end,
        do_insert(Type, Entry, Board, InsertMem);
    false ->
        ?WARNING("cannot find entry: ~p", [Dn])
    end.

insert_board(Type, Entry, Board) ->
    do_insert(Type, Entry, Board, ignore).

do_insert(Type, Entry, Board, CallFun) ->
    DeviceInfo = get_device_info(Type, Entry),
    BoardInfo = DeviceInfo ++ Board,
    case emysql:insert(mit_boards, [{created_at, {datetime, calendar:local_time()}}|BoardInfo]) of
        {updated,{1, Bid}} ->
            if is_function(CallFun) ->
                CallFun(Bid, BoardInfo);
             true ->
                 ok
            end;
        {updated, {0, _}} -> %stale board?
            ?WARNING("stale board: ~p", [BoardInfo]);
        {error, Reason} ->
            ?WARNING("~p", [Reason])
    end.

get_device_info(Type, Entry) ->
    {value, Id} = dataset:get_value(id, Entry),
    {value, CityId} = dataset:get_value(cityid, Entry,0),
    {value, DeviceManu} = dataset:get_value(device_manu, Entry,0),
    DeviceType = mit_util:get_type(Type),
    [{device_type, DeviceType}, {device_id, Id},{device_manu,DeviceManu},{cityid,CityId}].


update_board(Dn, OldAttrs, Attrs) ->
    UpdateMem = fun(Id, BoardInfo) ->
         Uid = "slot:" ++ integer_to_list(Id),
         mit:update(#entry{dn = Dn, uid = Uid, type = board, parent = mit_util:bdn(Dn), data = BoardInfo})
     end,
    mit_util:do_update(mit_boards, Attrs, OldAttrs, UpdateMem).

update_board(OldAttrs, Attrs) ->
    mit_util:do_update(mit_boards, Attrs, OldAttrs, ingore).

