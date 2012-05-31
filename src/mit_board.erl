%%%----------------------------------------------------------------------
%%% File    : mit_board.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : olt board
%%% Created : 30 Nov 2009
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2007-2009, www.opengoss.com
%%%----------------------------------------------------------------------
-module(mit_board).

-author('ery.lee@gmail.com').

-import(extbif, [to_binary/1,to_list/1]).

-behavior(gen_server).

-include("mit.hrl").
-include_lib("elog/include/elog.hrl").

%start/stop
-export([start_link/0,
         stop/0]).

%api
-export([attrs/0,
         lookup/1,
		 add/2,
		 update/2]).

%callback
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3]).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @spec () -> ok
%% @doc Stop the mit server.
stop() ->
	gen_server:call(?SERVER, stop).

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

add(Dn, Attrs) ->
	gen_server:cast(?SERVER, {add, Dn, Attrs}).

update(Dn, Attrs) ->
	gen_server:cast(?SERVER, {update, Dn, Attrs}).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    case mnesia:system_info(extra_db_nodes) of
        [] -> %master node
            do_init();
        _ -> %slave node
            ok
    end,
    {ok, state}.
    

do_init() ->
    {ok, Boards} = emysql:select({mit_boards, attrs()}),
    lists:foreach(fun(Board) ->
        {value, DevId} = dataset:get_value(device_id, Board),
        {value, DevType} = dataset:get_value(device_type, Board),
        Uid = mit_util:uid(mit_util:get_type(DevType), DevId),
        case mit:lookup(id, Uid) of
        {ok, #entry{dn=Bdn} = _} ->
			{value, Id} = dataset:get_value(id, Board),
			Buid = "slot:" ++ integer_to_list(Id),
			{value, Boardid} = dataset:get_value(boardid, Board),
			Rdn = "slot=" ++ to_list(Boardid),
            Dn = Rdn ++ "," ++ to_list(Bdn),
            mit:update(#entry{dn = to_binary(Dn), uid = to_binary(Buid),parent = Bdn, type = board, data = Board});
        false ->
            ignore
        end
    end, Boards),
    io:format("finish start board : ~p ~n", [length(Boards)]),
    {ok, state}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
	{stop, normal, ok, State};

handle_call(Request, _From, State) ->
	?ERROR("unexpected requrest: ~n", [Request]),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({add, Dn, Attrs}, State) ->
    case lookup(to_binary(Dn)) of
    {ok, OldAttrs} ->
        update_board(Dn, OldAttrs, Attrs);
    false ->
        insert_board(Dn, Attrs)
    end,
    {noreply, State};

handle_cast({update, Dn, Attrs}, State) ->
    case lookup(to_binary(Dn)) of
    {ok, OldAttrs} ->
        update_board(Dn, OldAttrs, Attrs);
    false ->
        ?ERROR("cannot find board: ~p", [Dn])
    end,
    {noreply, State};

handle_cast(Msg, State) ->
	?ERROR("unexpected msg: ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

handle_info(Info, State) ->
    ?ERROR("unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
insert_board(Dn, Board) ->
    Bdn = mit_util:bdn(Dn),
    case mit:lookup(Bdn) of
    {ok, #entry{data = Entry, type = Type} = _} ->
        {value, Id} = dataset:get_value(id, Entry),
        {value, CityId} = dataset:get_value(cityid, Entry),
        {value, DeviceManu} = dataset:get_value(device_manu, Entry),
        DateTime = {datetime, calendar:local_time()},
        % ?INFO("insert_board: ~p", [Dn]),
        DeviceType = mit_util:get_type(Type),
        BoardInfo = [{device_type, DeviceType}, {device_id, Id},{device_manu,DeviceManu},
                {created_at, DateTime}, {updated_at, DateTime},{cityid,CityId} | Board],
        case emysql:insert(mit_boards, BoardInfo) of
        {updated,{1,Bid}} ->
                Uid = "slot:" ++ integer_to_list(Bid),
                mit:update(#entry{dn = Dn, uid = Uid, type = board, parent = mit_util:bdn(Dn), data = BoardInfo});
		{updated, {0, _}} -> %stale board?
	            ?WARNING("stale board: ~p,~p", [Dn,BoardInfo]);
        {error, Reason} ->
            ?WARNING("~p", [Reason])
        end;
    false ->
        ?WARNING("cannot find entry: ~p", [Bdn])
    end.

update_board(Dn, OldAttrs, Attrs) ->
    case mit_util:merge(Attrs, OldAttrs) of
    {changed, MergedAttrs} ->
        % ?INFO("update_board: ~p", [Dn]),
        {value, Id} = dataset:get_value(id, OldAttrs),
        Datetime = {datetime, calendar:local_time()},
        MergedAttrs2 = lists:keydelete(id, 1, MergedAttrs),
        case emysql:update(mit_boards, [{updated_at, Datetime} | MergedAttrs2], {id, Id}) of
        {updated, {1, _Id}} -> %update mit cache
            Uid = "slot:" ++ integer_to_list(Id),
            mit:update(#entry{dn = Dn, uid = Uid, type = board, parent = mit_util:bdn(Dn), data = MergedAttrs});
        {updated, {0, _}} -> %stale board?
            ?WARNING("stale board: ~p", [Dn]);
            %mit:delete(Dn);
        {error, Reason} ->
            ?ERROR("~p", [Reason])
        end;
    {unchanged, _} ->
        ok
    end.
