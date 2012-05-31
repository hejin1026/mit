%%%----------------------------------------------------------------------
%%% File    : mit_onu.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : onu mit
%%% Created : 30 Nov 2009
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2007-2009, www.opengoss.com
%%%----------------------------------------------------------------------
-module(mit_onu).

-author('ery.lee@gmail.com').

-include("mit.hrl").
-include_lib("elog/include/elog.hrl").

-import(extbif, [to_binary/1]).

-behavior(gen_server).

-export([start_link/0,
         stop/0]).

-export([all/0,
         one/1,
         redisco/0
         ]).

%api
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

-import(extbif, [to_list/1]).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, stop).




all() ->
    Sql = "select t2.means as means, t1.* ,'onu' device_type  from mit_onus t1 LEFT join collect_means t2 on
        (t1.cityid = t2.cityid and t1.device_manu = t2.device_manu) where t2.means is not null",
    get_data(Sql).

one(Id) ->
    Sql = "select t2.means as means, t1.* ,'onu' device_type  from mit_onus t1 LEFT join collect_means t2 on
        (t1.cityid = t2.cityid and t1.device_manu = t2.device_manu) where t2.means is not null and t1.id = " ++ to_list(Id),
    get_data(Sql).

redisco() ->
    Sql = "select t2.means as means, t1.*,'onu' device_type   from mit_onus t1 LEFT join collect_means t2 on
        (t1.cityid = t2.cityid and t1.device_manu = t2.device_manu) where t2.means is not null and t1.discovery_state = 2",
    get_data(Sql).

get_data(Sql) ->
    case emysql:sqlquery(Sql) of
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
     ip,
     cityid,
     sysoid,
     name,
	 splite_id,
     device_name,
     adminstate,
     onlinestatus,
     authmacaddr,
     authmacsn,
     collect_type,
     discovery_state,
     device_kind,
     device_manu,
     macaddr,
     olt_id,
     onu_no,
     onu_state,
     onu_type,
     port_no,
     pvlan,
     rdn,
     regsn,
     regstate,
     slot_no,
     upmaximumbw,
     downmaximumbw,
     splite_id,
     entrance_id,
     ponid,
     snmp_r,
     snmp_w,
     snmp_v
    ].



get_entry(Onu) ->
    mit_util:format(mit, mem_attrs(), Onu).

get_notify_entry(Onu) ->
    {value, OltId} = dataset:get_value(olt_id, Onu),
    {value, Rdn} = dataset:get_value(rdn, Onu),
    case mit:lookup(id, to_binary("olt:" ++ integer_to_list(OltId))) of
        {ok, #entry{data = Olt}} ->
            {value, OltIp} = dataset:get_value(ip, Olt),
            Dn = to_binary("onu=" ++ to_list(Rdn) ++ ",olt=" ++ to_list(OltIp)),
            {value, OltCommunity} = dataset:get_value(snmp_r, Olt),
            {value, OltVersion} = dataset:get_value(snmp_v, Olt, <<"v2c">>),
            {value, OltWriteCommunity} = dataset:get_value(snmp_w, Olt, <<"private">>),
            OltAttrs =  [{oltip, OltIp},{olt_snmp_r, OltCommunity},{olt_snmp_v, OltVersion}, {olt_snmp_w, OltWriteCommunity}],
            Attrs = mit_util:format(notify, attrs(), Onu),
            OltAttrs ++ [{dn, Dn}|Attrs];
        false ->
            []
    end.


lookup(Dn) ->
    case mit:lookup(Dn) of
        {ok, #entry{data = Onu}} ->
            {ok, Onu};
        false ->
            false
    end.

add(Dn, Onu) ->
	gen_server:cast(?MODULE, {add, Dn, Onu}).

update(Dn, Attrs) ->
    gen_server:cast(?MODULE, {update, Dn, Attrs}).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    case emysql:select({mit_onus, mem_attrs()}) of
        {ok, Onus} ->
            lists:foreach(fun(Onu) ->
                  {value, Id} = dataset:get_value(id, Onu),
                  {value, OltId} = dataset:get_value(olt_id, Onu),
                  Entry = case dataset:get_value(ip, Onu) of
                      {value, Ip} ->
                          #entry{uid = mit_util:uid(onu,Id), ip=Ip, type = onu, data = Onu};
                      {false, _} ->
                          #entry{uid = mit_util:uid(onu,Id), type = onu, data = Onu}
                   end,
                  {value, Rdn} = dataset:get_value(rdn, Onu),
                  case mit:lookup(id, to_binary("olt:" ++ integer_to_list(OltId))) of
                      {ok, #entry{dn = OltDn, data = Olt}} ->
                          ?INFO("insert onu :~p", [OltDn]),
                          {value, OltIp} = dataset:get_value(ip, Olt),
                          Dn = lists:concat(["onu=", to_list(Rdn), ",", "olt=", to_list(OltIp)]),
                          mit:update(Entry#entry{dn = to_binary(Dn), parent = OltDn});
                      false ->
                          ignore
                  end
          end, Onus),
          io:format("finish start onu : ~p ~n", [length(Onus)]),
          {ok, state};
        {error, Reason} ->
            ?ERROR("mit_onu start failure...~p",[Reason]),
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
handle_cast({add, Dn, Onu0}, State) ->
    Onu = transform(Onu0),
    case lookup(Dn) of
        {ok, OldOnu} ->
            update_onu(Dn, OldOnu, Onu);
        false ->
            insert_onu(Dn, Onu)
    end,
    {noreply, State};

handle_cast({update, Dn, Attrs}, State) ->
    case lookup(Dn) of
        {ok, OldAttrs} ->
            update_onu(Dn, OldAttrs, Attrs);
        false ->
            ?ERROR("cannot find onu ~p", [Dn])
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

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
update_onu(Dn, OldAttrs, Attrs) ->
    %?INFO("update onu,dn:~p, oldattr: ~p, newattr: ~p", [Dn, OldAttrs, Attrs]),
    case mit_util:merge(Attrs, OldAttrs) of
        {changed, MergedAttrs} ->
           % ?WARNING("update onu dn:~p,newattr: ~p ~n,result : ~p", [Dn, Attrs, MergedAttrs]),
            {value, Id} = dataset:get_value(id, OldAttrs, -1),
            {value, Ip} = dataset:get_value(ip, MergedAttrs, undefined),
            MergedAttrs1 = lists:keydelete(id, 1, MergedAttrs),
            Datetime = {datetime, calendar:local_time()},
            case emysql:update(mit_onus, [{updated_at, Datetime} | MergedAttrs1], {id, Id}) of
                {updated, {1, _Id}} -> %update mit cache
                    mit:update(#entry{dn = Dn, uid = mit_util:uid(onu,Id), ip= Ip,
                        type = onu, parent = mit_util:bdn(Dn), data = MergedAttrs});
                {updated, {0, _Id}} -> %stale onu?
                    ?WARNING("stale onu: ~p,~p", [Dn, Id]);
                {error, Reason} ->
                    ?ERROR("~p", [Reason])
            end;
        {unchanged, _} ->
            ok
    end.

insert_onu(Dn, Onu) ->
    case mit:lookup(mit_util:bdn(Dn)) of
        {ok, #entry{data = Olt, type = olt}} ->
            {value, OltId} = dataset:get_value(id, Olt),
            {value, CityId} = dataset:get_value(cityid, Olt),
            ?INFO("insert onu: ~p", [Dn]),
            Now = {datetime, calendar:local_time()},
            case emysql:insert(mit_onus, [{olt_id, OltId},{cityid, CityId},{created_at, Now}|Onu]) of
                {updated, {1, Id}} ->
               %     ?INFO("insert onu dn:~p,result: ~p", [Dn, Onu]),
                    {value, Ip} = dataset:get_value(ip, Onu, undefined),
                    mit:update(#entry{dn = to_binary(Dn), uid = mit_util:uid(onu,Id),ip=Ip,type = onu,
                        parent = mit_util:bdn(Dn),data = [{id, Id}|Onu]});
                {updated, {0, _}} ->
                    ?WARNING("cannot find inserted onu: ~p ~p", [Dn, Onu]);
                {error, Reason} ->
                    ?ERROR("OltId : ~p,dn :~p, Reason: ~p", [OltId,Dn, Reason]);
                _ ->
                    ok
            end;
        {ok, #entry{type = Type}} ->
            ?ERROR("cannot find :~p to olt: ~p", [Type, Dn]);
        false ->
            ?ERROR("cannot find olt: ~p", [Dn])
    end.

transform(Attrs) ->
    transform(Attrs, []).

transform([], Acc) ->
    Acc;
transform([{ip, Ip} | T], Acc) ->
    Ip1 = to_list(Ip),
    if Ip1 == "0.0.0.0" ->
            transform(T, Acc);
        Ip1 == "255.255.255.255" ->
            transform(T, Acc);
        true ->
            transform(T, [{ip, Ip}|Acc])
    end;
transform([{vendor, Vendor}|T], Acc) ->
    ManuId = mit_dict:lookup(vendor, Vendor),
    transform(T, [{device_manu, ManuId}|Acc]);
transform([{type,Type }|T], Acc) ->
    TypeId = mit_dict:lookup(type, Type),
    transform(T, [{device_kind, TypeId},{onu_type,to_binary(Type)}|Acc]);
transform([H|T], Acc) when is_list(H) ->
    transform(T, [to_binary(H) | Acc]);
transform([H|T], Acc) ->
    transform(T, [H | Acc]).

