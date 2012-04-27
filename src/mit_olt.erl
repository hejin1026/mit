%%%----------------------------------------------------------------------
%%% File    : mit_olt.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : olt management
%%% Created : 28 Nov 2009
%%% License : http://www.opengoss.com/
%%%
%%% Copyright (C) 2007-2009, www.opengoss.com
%%%----------------------------------------------------------------------
-module(mit_olt).

-author('ery.lee@gmail.com').

-include("mit.hrl").
-include_lib("elog/include/elog.hrl").

-import(extbif, [to_binary/1, to_list/1]).

-behavior(gen_server).

-export([start_link/0,
         stop/0]).

-export([all/0,
         one/1,
         redisco/0
         ]).

-export([attrs/0,
         lookup/1,
         get_entry/1,
         get_notify_entry/1,
         add/2,
         update/2]).

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

stop() ->
	gen_server:call(?SERVER, stop).



all() ->
    Sql = "select t2.means as means, t1.*,'olt' device_type   from mit_olts t1 LEFT join collect_means t2 on
        (t1.cityid = t2.cityid and t1.device_manu = t2.device_manu) where t2.means is not null",
    get_data(Sql).

one(Id) ->
    Sql = "select t2.means as means, t1.* ,'olt' device_type  from mit_olts t1 LEFT join collect_means t2 on
        (t1.cityid = t2.cityid and t1.device_manu = t2.device_manu) where t2.means is not null and t1.id = " ++ to_list(Id),
    get_data(Sql).

redisco() ->
    Sql = "select t2.means as means, t1.* ,'olt' device_type  from mit_olts t1 LEFT join collect_means t2 on
        (t1.cityid = t2.cityid and t1.device_manu = t2.device_manu) where t2.means is not null and t1.discovery_state = 2",
    get_data(Sql).

get_data(Sql) ->
    case emysql:sql_query(Sql) of
        {ok, Records} ->
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
     avail_status,
     is_discovery,
     discovery_state,
     olt_state,
     snmp_r,
     snmp_w,
     snmp_v
    ].


get_entry(Olt) ->
    mit:format(mit, mem_attrs(), Olt).

get_notify_entry(Olt) ->
    {value, Ip} = dataset:get_value(ip, Olt),
	Dn = "olt=" ++ to_list(Ip),
    Attrs = mit:format(notify, attrs(), Olt),
     [{dn, list_to_binary(Dn)}|Attrs].


lookup(Dn) ->
    case mit:lookup(Dn) of
    {ok, #entry{data = Olt}} ->
        {ok, Olt};
    false ->
        false
    end.

add(Dn, Attrs) ->
    gen_server:cast(?SERVER, {add, Dn, Attrs}).

update(Dn, Attrs) ->
    gen_server:cast(?SERVER, {update, Dn, Attrs}).


%%====================================================================
%% gen_server callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    case emysql:select(mit_olts, mem_attrs()) of
        {ok, Olts} ->
            lists:foreach(fun(Olt) ->
                {value, Id} = dataset:get_value(id, Olt),
                {value, Ip} = dataset:get_value(ip, Olt),
                Dn = "olt=" ++ to_list(Ip),
                Entry = #entry{dn = to_binary(Dn), uid = mit:uid(olt,Id), type = olt, parent = undefined, data = Olt},
                mit:update(Entry)
            end, Olts),
            {ok, state};
        {error, Reason} ->
            ?ERROR("start failure...~p",[Reason]),
            {stop, Reason}
    end.

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
    {reply, {error, unexpected_request}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({add, Dn, Attrs}, State) ->
    Olt = transform(Attrs),
    case mit:lookup(Dn) of
        {ok, #entry{data = Entry} = _} ->
            update_olt(Dn, Entry, Olt);
        false ->
            insert_olt(Dn, Olt)
    end,
    {noreply, State};

handle_cast({update, Dn, Attrs}, State) ->
    case mit:lookup(Dn) of
    {ok, #entry{data = Entry} = _} ->
        update_olt(Dn, Entry, Attrs);
    false ->
        ?ERROR("cannot find olt ~p", [Dn])
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
insert_olt(Dn, Olt) ->
    CreatedAt = {datetime, calendar:local_time()},
    case emysql:insert(mit_olts, [{created_at, CreatedAt}|Olt]) of
    {updated, {1, Id}} ->
        mit:update(#entry{dn = to_binary(Dn), uid = mit:uid(olt,Id), type = olt, data = [{id, Id}|Olt]});
    {updated, {0, _}} ->
        ?WARNING("cannot find inserted olt: ~p ~p", [Dn, Olt]);
    {error, Reason} ->
        ?ERROR("dn :~p, Reason: ~p", [Dn, Reason]);
    _ ->
        ok
    end.

update_olt(Dn, OldAttrs, Attrs) ->
   %   ?INFO("update olt: ~p，~p", [Dn,Attrs]),
      case mit:merge(Attrs, OldAttrs) of
        {changed, MergedAttrs} ->
            {value, Id} = dataset:get_value(id, OldAttrs, -1),
            MergedAttrs1 = lists:keydelete(id, 1, MergedAttrs),
            Datetime = {datetime, calendar:local_time()},
            case emysql:update(mit_olts, [{updated_at, Datetime} | MergedAttrs1], {id, Id}) of
            {updated, {1, _Id}} -> %update mit cache
                mit:update(#entry{dn = Dn, uid = mit:uid(olt,Id), type = olt, parent = mit:bdn(Dn), data = MergedAttrs});
            {updated, {0, _Id}} ->
                ?WARNING("stale Olt: ~p", [Dn]);
            {error, Reason} ->
                ?ERROR("~p", [Reason])
            end;
        {unchanged, _} ->
            ok
    end.


transform(Attrs) ->
    transform(Attrs, []).

transform([{vendor, Vendor}|T], Acc) ->
    ManuId = mit_dict:lookup(vendor, Vendor),
    transform(T, [{device_manu, ManuId}|Acc]);
transform([{type, Type}|T], Acc) ->
	TypeId = mit_dict:lookup(type, Type),
	transform(T, [{device_kind, TypeId},{olt_type,Type},{olt_state,1}|Acc]);
transform([H|T], Acc) ->
    transform(T, [H | Acc]);

transform([], Acc) ->
    Acc.


