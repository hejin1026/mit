-module(mit).

-created("hejin 2012-4-26").

-include_lib("elog/include/elog.hrl").
-include("mit.hrl").

-import(extbif, [to_list/1, to_binary/1]).

-behavior(gen_server).

-export([start_link/0,
         stop/0]).

%api
-export([lookup_entry/1,
         lookup/1,
         lookup/2,
         update/1,
         delete/2]).

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

stop() ->
	gen_server:call(?SERVER, stop).

%%compatible with old entry
lookup_entry(Dn) ->
    case lookup(dn, Dn) of
    {ok, #entry{type = Type, data = Obj} = _} ->
        case Type of
            onu ->
                {ok, mit_util:notify_entry(onu, Obj)};
            olt ->
                {ok, mit_util:notify_entry(olt, Obj)};
            port ->
                {ok, Obj};
            board ->
                {ok, Obj};
            _Other ->
               {ok, Obj}
        end;
    false ->
        false
    end.

lookup({ip, Ip}) ->
    lookup(ip, Ip);
lookup({id, Id}) ->
    lookup(id, Id);
lookup(Dn) ->
    lookup(dn, Dn).

lookup(dn, Dn) ->
    case mnesia:dirty_read(entry, to_binary(Dn)) of
    [Entry] ->
        {ok, Entry};
    [] ->
        false
    end;
lookup(id, Uid) ->
    case mnesia:dirty_index_read(entry, Uid, #entry.uid) of
    [Entry] ->
        {ok, Entry};
    [Entry|Entry1] ->
        ?WARNING("more than one entry for one uid: ~p,~p,~p~n", [Uid,Entry,Entry1]),
        false;
    [] ->
        false
    end;
lookup(ip, Ip) ->
    case mnesia:dirty_index_read(entry, Ip, #entry.ip) of
    [Entry] ->
        {ok, Entry};
    [_Entry|_] ->
        false;
    [] ->
        false
    end.


update(AtomType, Dn, Id, Ip, Data) ->
    update(#entry{dn = Dn, uid = mit_util:uid(AtomType, Id), ip= Ip, parent = mit_util:bdn(Dn),
        type = AtomType, data = mit_util:mit_entry(AtomType, Data)}).

update(Entry) when is_record(Entry, entry) ->
  Reason =  mnesia:sync_dirty(fun() ->
        mnesia:write(Entry)
    end),
    case Reason of
        ok -> ok;
        _ -> ?ERROR("Reason~p",[Reason])
        end.

delete(id, Uid) ->
    Entries = mnesia:dirty_index_read(entry, Uid, #entry.uid),
    mnesia:sync_dirty(fun() ->
        [mnesia:delete_object(Entry) || Entry <- Entries]
    end);

delete(dn, Dn) ->
    Children = mnesia:dirty_index_read(entry, Dn, #entry.parent),
    mnesia:sync_dirty(fun() ->
        [mnesia:delete_object(Child) || Child <- Children],
        mnesia:delete({entry, Dn})
    end).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    ?ERROR("starting  mit...~n",[]),
    case mnesia:system_info(extra_db_nodes) of
        [] -> %master node
            emysql:delete(mit_devices_changed),
            {atomic, ok} = mnesia:create_table(entry,
                [{ram_copies, [node()]}, {index, [uid, ip, parent]},
                 {attributes, record_info(fields, entry)}]),

            BootSteps = [{Name, Mod, Fun, Descr, Dep}
                                || {Mod, [{Name, Fun, Descr, Dep}]}
                                    <- extlib:module_with_attrs(mit, mit_boot_load)],
            [put({boot_step, element(1, Step)}, Step) || Step <- BootSteps],
            [boot_load_step(Step) || Step <- BootSteps],
            erlang:send_after(120 * 1000, self(), sync_changes);
         V -> %slave node
            ?INFO("im slave node:~p",[V]),
            ok
    end,
    ?ERROR("finish start mit...~n",[]),
    mnesia:add_table_copy(entry, node(), ram_copies),
    ?ERROR("finish copy mit...~n",[]),
    {ok, state}.

boot_load_step({Name, Mod, Fun, Descr, Dep}) ->
	case get({boot_load, Name}) of
	true ->
		ok;
	_ ->
		DepLoaded = get({boot_load, Dep}),
        ?ERROR("begin to load ~s", [Name]),
		if
		Dep =:= undefined ->
			ok;
		DepLoaded ->
			ok;
		true ->
			boot_load_step(get({boot_step, Dep}))
		end,
		?ERROR("end with load ~p, ~s", [Name, Descr]),
		Mod:Fun(),
		put({boot_load, Name}, true)
	end.


handle_call(stop, _From, State) ->
	{stop, normal, ok, State};

handle_call(Req, _From, State) ->
	?ERROR("unexpected requrest: ~p", [Req]),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(sync_changes, State) ->
	case emysql:select(mit_devices_changed) of
        {ok, ChangedLogs} ->
            lists:foreach(fun(ChangedLog) ->
                ?ERROR("sync_change: ~p ", [ChangedLog]),
                {value, Id} = dataset:get_value(id, ChangedLog),
                {value, DevId} = dataset:get_value(device_id, ChangedLog),
                {value, DevType} = dataset:get_value(device_type, ChangedLog),
                {value, Oper} = dataset:get_value(oper_type, ChangedLog),
                try handle_change(Oper, {mit_util:get_type(DevType), DevId}, ChangedLog) of
                    _ -> emysql:delete(mit_devices_changed, {id, Id})
                catch
                    _:Ex -> ?ERROR("~p, ~p", [Ex, erlang:get_stacktrace()])
                end
            end, ChangedLogs);
        {error, Reason} ->
            ?ERROR("~p", [Reason])
	end,
	erlang:send_after(10 * 1000, self(), sync_changes),
    {noreply, State};

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
handle_change(_, undefined, ChangedLog) ->
    ?ERROR("dn is undefined: ~p.", [ChangedLog]);

handle_change(?MIT_ADD, {AtomType, DevId}, _ChangedLog) ->
    ?ERROR("mit add :~p", [{AtomType, DevId}]),
    Callback = fun(Dn, Entry) -> mit_event:notify({insert, Dn, Entry}) end,
    do_handle_change(AtomType, DevId, Callback);

handle_change(?MIT_UPDATE, {AtomType, DevId}, _ChangedLog) ->
    Callback = fun(Dn, Entry) -> mit_event:notify({update, Dn, Entry}) end,
    do_handle_change(AtomType, DevId, Callback);

handle_change(?MIT_DELETE, {AtomType, DevId}, _ChangedLog) ->
    Uid = mit_util:uid(AtomType, DevId),
    case lookup(id, Uid) of
    {ok, #entry{dn = Dn} = _Entry} ->
        ?ERROR("mit delete :~p", [{AtomType, DevId}]),
        mit_event:notify({delete, Dn}),
        delete(dn, Dn);
    false ->
        ok
    end;

handle_change(Oper, {AtomType, DevId}, _ChangedLog) ->
    Uid = mit_util:uid(AtomType, DevId),
    ?ERROR("unexpected oper ~p on ~p ", [Oper, Uid]).

do_handle_change(AtomType, DevId, Callback) ->
    case mit_util:get_entry(AtomType, DevId) of
        [Obj|_] ->
            NotifyEntry = mit_util:notify_entry(AtomType, Obj),
            {value, Dn} = dataset:get_value(dn, NotifyEntry),
            {value, Ip} = dataset:get_value(ip, NotifyEntry),
            %% equal to mit_event_h
            update(AtomType, Dn, DevId, Ip, Obj),
            ?INFO("mit_update,entry:~p,~n obj:~p",[NotifyEntry,Obj]),
            Callback(Dn, NotifyEntry);
        [] ->
            ?ERROR("cannot find entry: ~p", [{DevId, AtomType}])
    end.
