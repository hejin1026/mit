%%%----------------------------------------------------------------------
%%% File    : mit.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : management information tree
%%% Created : 21 Feb 2008
%%% Updated : 23 Nov 2009
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2007-2009, www.opengoss.com
%%%----------------------------------------------------------------------
-module(mit).

-author('ery.lee@gmail.com').

-include_lib("elog/include/elog.hrl").
-include("mit.hrl").

-import(extbif, [to_list/1, to_binary/1]).

-behavior(gen_server).

-export([start_link/0,
         stop/0]).

%utility functions
-export([bdn/1,
         rdn/1,
		 uid/2,
         get_type/1,
         format/3,
         notify_entry/2,
         mit_entry/2
         ]).

%utility functions
-export([merge/2]).

%api
-export([lookup_entry/1,
         lookup/1,
         lookup/2,
         update/1,
         delete/1,
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

%% @spec () -> ok
%% @doc Stop the mit server.
stop() ->
	gen_server:call(?SERVER, stop).

%%compatible with old entry
lookup_entry(Dn) ->
    case lookup(dn, Dn) of
    {ok, #entry{type = Type, data = Obj} = _} ->
        case Type of
            onu ->
                {ok, notify_entry(onu, Obj)};
            olt ->
                {ok, notify_entry(olt, Obj)};
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

%% @spec (Dn) -> {ok, Entry} | false
%%  Dn = string()
%%  Redord = record()
%% @doc lookup a mit entry by dn
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

update({Dn, Uid, Data}) ->
    gen_server:call(?MODULE,
            {update, #entry{dn = Dn, uid = Uid, parent = bdn(Dn), data = Data}});

update(Entry) when is_record(Entry, entry) ->
    gen_server:call(?MODULE, {update, Entry}).

delete(Dn) ->
    delete(dn, Dn).

delete(dn, Dn) ->
    gen_server:cast(?MODULE, {delete, dn, Dn});

delete(id, Uid) ->
    gen_server:cast(?MODULE, {delete, uid, Uid}).

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
    mysql:delete(mit_devices_changed),
    erlang:send_after(120 * 1000, self(), sync_changes),
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
handle_call({update, Entry}, _From, State) ->
    Reply = mnesia:sync_dirty(fun() ->
        mnesia:write(Entry)
    end),
    {reply, Reply, State};

handle_call(stop, _From, State) ->
	{stop, normal, ok, State};

handle_call(Req, _From, State) ->
	?ERROR("unexpected requrest: ~p", [Req]),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({delete, dn, Dn}, State) ->
    mnesia:sync_dirty(fun() ->
        mnesia:delete({entry, Dn})
    end),
    {noreply, State};

handle_cast({delete, id, Uid}, State) ->
    Entries = mnesia:dirty_index_read(entry, Uid, #entry.uid),
    mnesia:sync_dirty(fun() ->
        [mnesia:delete_object(Entry) || Entry <- Entries]
    end),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(sync_changes, State) ->
	case mysql:select(mit_devices_changed) of
    {ok, ChangedLogs} ->
        lists:foreach(fun(ChangedLog) ->
            ?ERROR("sync_change: ~p ", [ChangedLog]),
            {value, Id} = dataset:get_value(id, ChangedLog),
            {value, DevId} = dataset:get_value(device_id, ChangedLog),
            {value, DevType} = dataset:get_value(device_type, ChangedLog),
            {value, Oper} = dataset:get_value(oper_type, ChangedLog),
            try handle_change(Oper, {get_type(DevType), DevId}, ChangedLog) of
                _ -> mysql:delete(mit_devices_changed, {id, Id})
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
%% @spec bdn(Dn) -> Bdn
%%  Bd = binary()
%%  Bdn = binary()
%% @doc lookup a mit entry by dn
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
    ?ERROR("mit delete :~p", [{AtomType, DevId}]),
    Uid = uid(AtomType, DevId),
    case lookup(id, Uid) of
    {ok, #entry{dn = Dn} = Entry} ->
        ?INFO("mit delete: ~p", [Dn]),
        mit_event:notify({delete, Dn, Entry}),
        Children = mnesia:dirty_index_read(entry, Dn, #entry.parent),
        mnesia:sync_dirty(fun() ->
            [mnesia:delete_object(Child) || Child <- Children],
            mnesia:delete({entry, Dn})
        end);
    [] ->
        ok
    end;

handle_change(Oper, {AtomType, DevId}, _ChangedLog) ->
    Uid = uid(AtomType, DevId),
    ?ERROR("unexpected oper ~p on ~p ", [Oper, Uid]).

do_handle_change(AtomType, DevId, Callback) ->
    case get_entry(AtomType, DevId) of
        [Obj|_] ->
            NotifyEntry = notify_entry(AtomType, Obj),
            {value, Dn} = dataset:get_value(dn, NotifyEntry),
            Entry = #entry{dn = Dn, uid = uid(AtomType, DevId), parent = mit:bdn(Dn),
                type = AtomType, data = mit_entry(AtomType, Obj)},
            mnesia:sync_dirty(fun() ->
                mnesia:write(Entry)
            end),
            ?INFO("mit_update,entry:~p,obj:~p",[NotifyEntry,Obj]),
            Callback(Dn, NotifyEntry);
        [] ->
            ?ERROR("cannot find entry: ~p", [{DevId, AtomType}])
    end.

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

