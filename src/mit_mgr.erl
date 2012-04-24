%%%----------------------------------------------------------------------
%%% File    : mit_mgr.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : 
%%% Created : 18 Feb 2008
%%% Updated : 08 Dec 2009
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2007-2009, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(mit_mgr).

-include("mit.hrl").

-include("elog.hrl").

-behavior(gen_server).

-export([start_link/0, sync/0, sync_entry/2]).

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

sync() ->
    gen_server:cast(?SERVER, sync).

sync_entry(Type, Id) ->
    gen_server:cast(?SERVER, {sync_entry, Type, Id}).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
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
handle_call(Request, _From, State) ->
    ?ERROR("unexpected request: ~p", [Request]),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(sync, State) ->
    sync(olt, mit_olt:all()),
    sync(olt, mit_onu:all()),
    sync(port, mit_port:all_monet()),
    {noreply, State};

handle_cast({sync_entry, Type, Id}, State) ->
    case Type of
        onu ->  sync(onu, mit_onu:one(Id));
        olt ->  sync(olt, mit_olt:one(Id));
        port -> sync(port, mit_port:one(Id));
        _ ->    ignore
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
sync(Type, Records) ->
    ?ERROR("sync ~p ~p", [Type, length(Records)]),
    lists:foreach(fun(Record) ->
        try do_sync_entry(Type, Record)
        catch
            _:Err -> ?ERROR("record: ~p, error: ~p, ~p", [Record, Err, erlang:get_stacktrace()])
        end,
        timer:sleep(20)
    end, Records).

do_sync_entry(port, Record) ->
    Entry = mit:notify_entry(port, Record),
    {value, Dn} = dataset:get_value(dn, Entry),              
    ?INFO("sync port ~p", [Dn]),
    monet_server:monitor_entry(Dn, Entry);

do_sync_entry(Type, Record) ->
    {value, Id} = dataset:get_value(id, Record),
    Entry = mit:notify_entry(Type, Record),
    {value, Dn} = dataset:get_value(dn, Entry),
    mit:update(#entry{dn = Dn, uid = mit:uid(Type, Id), parent = mit:bdn(Dn), type = Type, data = mit:mit_entry(Type, Record)}),
    ?INFO("sync event ~p", [Dn]),
    mit_event:notify({present, Dn, Entry}).
