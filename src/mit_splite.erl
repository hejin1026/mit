-module(mit_splite).

-author("hejin 2011-3-3").

-include("mit.hrl").
-include_lib("elog/include/elog.hrl").

-behavior(gen_server).

-export([start_link/0,
         stop/0]).

%api
-export([attrs/0,
         lookup/1,add/2]).

-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3]).

-import(extbif, [to_list/1, to_binary/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add(Dn, Attrs) ->
    gen_server:cast(?MODULE, {add, Dn, Attrs}).

stop() ->
	gen_server:call(?MODULE, stop).

attrs() ->
    [
        id,
        parent_split,
        split_name,
        olt_id,
        pon_id
    ].


lookup(Dn) ->
    case mit:lookup(Dn) of
    {ok, #entry{data = Splite, type = splite}} ->
        {ok, Splite};
    false ->
        false;
    _Other ->
        false
    end.


init([]) ->
    ?INFO("start mit splite...",[]),
    case mnesia:system_info(extra_db_nodes) of
        [] -> %master node
            do_init();
        _ -> %slave node
            ok
    end,
    {ok, state}.


do_init() ->
    case emysql:select({mit_splites, attrs()}) of
    {ok, Splites} ->
        lists:foreach(fun(Splite) ->
            {value, Id} = dataset:get_value(id, Splite),
            {value, ParentId} = dataset:get_value(parent_split, Splite, null),
            {value, PonId} = dataset:get_value(pon_id, Splite, null),
            SpliteUid = mit_util:uid(splite, Id),
            case mit:lookup(id, to_binary("port:" ++ to_list(PonId))) of
                {ok, #entry{dn = PonDn}} ->
                    SpliteDn = "splite=" ++ to_list(Id) ++ "," ++  to_list(PonDn),
                    case ParentId of
                        null ->
%                            ?INFO("level 1 splite :~p, ~p", [SpliteDn, Splite]),
                            mit:update(#entry{dn = to_binary(SpliteDn), uid = SpliteUid,
                                type = splite, parent = mit_util:bdn(SpliteDn), data = Splite});
                        _ ->
                            ParentDn = "splite=" ++ to_list(ParentId) ++ "," ++ to_list(PonDn),
                            mit:update(#entry{dn = to_binary(SpliteDn), uid = SpliteUid,
                                type = splite, parent = to_binary(ParentDn), data = Splite})
                    end;
                 _ ->
                     ignore
             end
        end, Splites),
        {ok, state};
    {error, Reason} ->
        {stop, Reason}
    end.

handle_call(stop, _From, State) ->
	{stop, normal, ok, State};

handle_call(Request, _From, State) ->
	?ERROR("unexpected requrest: ~n", [Request]),
    {reply, {error, unexpected_request}, State}.

handle_cast({add, Dn, Splite}, State) ->
     insert_splite(Dn, Splite),
    {noreply, State};

handle_cast(Msg, State) ->
	?ERROR("unexpected msg: ~p", [Msg]),
    {noreply, State}.

handle_info(update, State) ->
    do_init(),
    timer:send_after(30 * 60 * 1000, self(), update),
    {noreply, State};

handle_info(Info, State) ->
    ?ERROR("unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


insert_splite(PonDn,Splite)->
	?INFO(" insert_splite: ~p ~p", [PonDn, Splite]),
     case emysql:insert(mit_splites, Splite) of
            {updated, {0, _}} ->
                  ?WARNING("cannot inserted Splite: ~p ~p", [PonDn, Splite]);
            {updated, {1, PId}} ->
                  SpliteDn = "splite=" ++ to_list(PId) ++ "," ++  to_list(PonDn),
                  mit:update(#entry{dn = to_binary(SpliteDn), uid = mit_util:uid(splite,PId),
									type = splite, parent = PonDn, data = [{id, PId}|Splite]});
            {error, Reason} ->
                  ?WARNING("~p", [Reason])
      end.
