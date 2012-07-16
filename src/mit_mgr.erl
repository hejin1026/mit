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
-include_lib("elog/include/elog.hrl").

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

handle_call(Request, _From, State) ->
    ?ERROR("unexpected request: ~p", [Request]),
    {reply, ok, State}.

handle_cast(sync, State) ->
    sync(olt, mit_olt:all()),
    sync(onu, mit_onu:all()),
    sync(port, mit_port:all_monet()),
    {noreply, State};

handle_cast({sync_entry, Type, Id}, State) ->
    case Type of
        onu ->  sync(onu, mit_onu:one(Id));
        olt ->  sync(olt, mit_olt:one(Id));
        port -> sync(port, mit_port:one(Id));
        eoc -> sync(eoc, mit_eoc:one(Id));
        cpe -> sync(cpe, mit_cpe:one(Id));
        _ ->    ignore
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

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
    Entry = mit_util:notify_entry(port, Record),
    {value, Dn} = dataset:get_value(dn, Entry),
    ?INFO("sync port ~p", [Dn]),
    master_dist:monitor(Dn, Entry);

do_sync_entry(Type, Record) ->
    {value, Id} = dataset:get_value(id, Record),
    {value, Ip} = dataset:get_value(ip, Record, undefined),
    Entry = mit_util:notify_entry(Type, Record),
    {value, Dn} = dataset:get_value(dn, Entry),
    mit:update(#entry{dn = Dn, uid = mit_util:uid(Type, Id), ip= Ip, parent = mit_util:bdn(Dn),
        type = Type, data = mit_util:mit_entry(Type, Record)}),
    ?INFO("sync event ~p", [Dn]),
    mit_event:notify({present, Dn, Entry}).
