%%%----------------------------------------------------------------------
%%% File    : mit_dict.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose :
%%% Created : 25 Nov 2009
%%% Created : 07 Dec 2009
%%% License : http://www.opengoss.com/
%%%
%%% Copyright (C) 2007-2009, www.opengoss.com
%%%----------------------------------------------------------------------
-module(mit_dict).

-author('ery.lee@gmail.com').

-include("elog.hrl").

-import(extbif, [to_binary/1]).

-behavior(gen_server).

-export([start_link/0,
         lookup/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-define(SERVER, ?MODULE).

lookup(vendor, Name) when is_list(Name) or is_binary(Name)  ->
    case ets:lookup(mit_dic_manuid, to_binary(Name)) of
    [{_, Id}] -> Id;
    [] ->
        ?WARNING("unknown vendor: ~p", [Name]),
        0
    end;

lookup(vendor, Id) when is_integer(Id) ->
    case ets:lookup(mit_dic_idmanu, Id) of
    [{Id, Name}] -> Name;
    [] -> <<"null">>
    end;
lookup(type, Name) when is_list(Name) or is_binary(Name)  ->
	 case ets:lookup(mit_dic_typeid, to_binary(Name)) of
	 [{_, Id}] -> Id;
    [] ->
       ?WARNING("unknown kind: ~p", [Name]),
	       0
   end;

lookup(type, Id) when is_integer(Id) ->
	 case ets:lookup(mit_dic_idtype, Id) of
	 [{Id, Name}] -> Name;
	 [] -> <<"null">>
   end.

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    ets:new(mit_dic_typeid, [set, protected, named_table]),
    ets:new(mit_dic_idtype, [set, protected, named_table]),
    ets:new(mit_dic_manuid, [set, protected, named_table]),
    ets:new(mit_dic_idmanu, [set, protected, named_table]),
    ets:new(mit_dic_tid2vid, [set, protected, named_table]),
    init_cache(),
    {ok, state}.

init_cache() ->
	{ok, Types} = mysql:select(dic_device_types, [id, manufacturer_id, code_name]),
    {ok, Manus} = mysql:select(dic_manufacturers, [id, code_name]),
    store(mit_dic_tid2vid, Types),
    store(mit_dic_typeid, mit_dic_idtype, Types),
    store(mit_dic_manuid, mit_dic_idmanu, Manus),
    erlang:send_after(30*60*1000, self(), update_cache).

store(mit_dic_tid2vid, Types) ->
    lists:foreach(fun(Type) ->
        {value, Tid} = dataset:get_value(id, Type),
        {value, Vid} = dataset:get_value(manufacturer_id, Type),
        ets:insert(mit_dic_tid2vid, {Tid, Vid})
    end, Types).

store(Name2Id, Id2Name, Records) ->
   lists:foreach(fun(Record) ->
       {value, Id} = dataset:get_value(id, Record),
       case dataset:get_value(code_name, Record) of
       {value, CodeName} ->
           ets:insert(Name2Id, {CodeName, Id}),
           ets:insert(Id2Name, {Id, CodeName});
       false -> ignore;
       {false, _} ->
           ?WARNING("dic has no code_name: ~p", [Id])
       end
    end, Records).

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(update_cache, State) ->
    init_cache(),
    {noreply, State};

handle_info(Info, State) ->
    ?WARNING("unexpected inf: ~p", [Info]),
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


