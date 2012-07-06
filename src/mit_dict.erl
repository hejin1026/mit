-module(mit_dict).

-created("hejin 2012-7-5").

-include_lib("elog/include/elog.hrl").

-import(extbif, [to_list/1, to_binary/1]).

-behavior(gen_server).

-export([start_link/0,
         lookup/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(dict, {dn, value, type}).


lookup(vendor, Name)  ->
    Dn = lists:concat([vendor, "=", to_list(Name)]),
    lookup(Dn);
lookup(type, Name) ->
    Dn = lists:concat([type, "=", to_list(Name)]),
    lookup(Dn).

lookup(Dn) ->
    case mnesia:dirty_read(dict, Dn) of
        [Dict] ->
            {Dict#dict.value};
        [] ->
            []
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
            {atomic, ok} = mnesia:create_table(dict,
                [{ram_copies, [node()]}, {index, [id, name]},
                 {attributes, record_info(fields, dict)}]),
            init_cache(),
            io:format("finish start mit dict...~n",[]);
        _ -> %slave node
            ok
    end,
    {ok, state}.

init_cache() ->
	{ok, Types} = emysql:select({dic_device_types, [id, manufacturer_id, code_name]}),
    {ok, Manus} = emysql:select({dic_manufacturers, [id, code_name]}),
    store(type, Types),
    store(vendor, Manus),
    erlang:send_after(30*60*1000, self(), update_cache).

store(Type, Records) ->
   lists:foreach(fun(Record) ->
       {value, Id} = dataset:get_value(id, Record),
       case dataset:get_value(code_name, Record) of
       {value, CodeName} ->
           IdDn = lists:concat([Type, "=", Id]),
           NameDn = lists:concat([Type, "=", binary_to_list(CodeName)]),
           mnesia:sync_dirty(fun() ->
                mnesia:write(#dict{dn=IdDn, value=CodeName, type=Type}),
                mnesia:write(#dict{dn=NameDn, value=Id, type=Type})
            end);
       {false, _} ->
           ?WARNING("dic has no code_name: ~p", [Id])
       end
    end, Records).

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(update_cache, State) ->
    init_cache(),
    {noreply, State};

handle_info(Info, State) ->
    ?WARNING("unexpected inf: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


