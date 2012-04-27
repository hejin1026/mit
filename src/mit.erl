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
            Other ->
               ?WARNING("other entry:~p, ~p", [Dn, Other]),
               {ok, Obj}
        end;
    false ->
	   ?WARNING("no entry found:~p", [Dn]),
        false
    end.

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
    [Entry|_] ->
        ?WARNING("more than one entry for one uid: ~p", [Uid]),
        {ok, Entry};
    [] ->
        false
    end.

update(AtomType, Dn, Id, Data) ->
    Entry = #entry{dn = Dn, uid = mit_util:uid(AtomType, Id), parent = mit:bdn(Dn),
                type = AtomType, data = mit_util:mit_entry(AtomType, Data)},
    update(Entry).

update(Entry) when is_record(Entry, entry) ->
    mnesia:sync_dirty(fun() ->
        mnesia:write(Entry)
    end).

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
    {atomic, ok} = mnesia:create_table(entry,
        [{ram_copies, [node()]}, {index, [uid, parent]},
         {attributes, record_info(fields, entry)}]),
    mnesia:add_table_copy(entry, node(), ram_copies),
    emysql:delete(mit_devices_changed),
    erlang:send_after(120 * 1000, self(), sync_changes),
    {ok, state}.


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
    {ok, #entry{dn = Dn} = Entry} ->
        ?ERROR("mit delete :~p", [{AtomType, DevId}]),
        mit_event:notify({delete, Dn, Entry}),
        delete(dn, Dn);
    [] ->
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
            %% equal to mit_event_h
            update(AtomType, Dn, DevId, Obj),
            ?INFO("mit_update,entry:~p,obj:~p",[NotifyEntry,Obj]),
            Callback(Dn, NotifyEntry);
        [] ->
            ?ERROR("cannot find entry: ~p", [{DevId, AtomType}])
    end.
