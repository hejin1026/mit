-module(mit_area).

-author('chibj.opengoss@gmail.com').

-behavior(gen_server).

-export([find_area/1, find_area_id/1,
        lookup/1, lookup/2]).

-export([start_link/0, stop/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("mit.hrl").
-include_lib("elog/include/elog.hrl").

-record(area, {id, dn, name, type, parent_id}).

lookup(Id) ->
   	lookup(id,Id).

lookup(id,Id) ->
   	case mnesia:dirty_read(areas, Id) of
        [Area] ->
            Area;
        [] ->
            false
    end;
lookup(dn,Dn) ->
    case mnesia:dirty_index_read(areas, Dn, #area.dn) of
        [Area] ->
            Area;
        [_Area|_] ->
            false;
        [] ->
            false
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop).

init([]) ->
    case mnesia:system_info(extra_db_nodes) of
        [] -> %master node
            {atomic, ok} = mnesia:create_table(areas,
                       [{ram_copies, [node()]},
                        {attributes, record_info(fields, area)}]),
            load_areas(),
		    erlang:send_after(30*60*1000, self(), load_areas),
            io:format("finish start mit areas...~n",[]);
        _ -> %slave node
            ok
    end,
    {ok, state}.

load_areas() ->
    case emysql:select(areas) of
        {ok, Areas} ->
		 	?INFO("areas ~p",[Areas]),
            lists:foreach(fun(Area) ->
                      {value, Dn} = dataset:get_value(area_dn, Area),
                      {value, Id} = dataset:get_value(id, Area),
                      {value, Name} = dataset:get_value(area_name, Area),
                      {value, Level} = dataset:get_value(area_level, Area),
                      {value, ParentId} = dataset:get_value(parent_id, Area, -1),
                      mnesia:sync_dirty(fun() ->
                                mnesia:write(#area{dn=Dn, id=Id, name=Name, type=area_type(Level), parent_id = ParentId})
                        end)
              end, Areas);
        {error, Reason} ->
            ?ERROR("load areas failure...~p",[Reason])
    end.


handle_call(Req, _From, State) ->
    ?ERROR("Unexpected requrest: ~p", [Req]),
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(load_areas, State) ->
    load_areas(),
    erlang:send_after(60*60*1000, self(), load_areas),
    {noreply, State};

handle_info(Info, State) ->
    ?ERROR("Unexpected info: ~p", [Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
area_type(0) -> province;
area_type(1) -> city;
area_type(2) -> town;
area_type(3) -> branch;
area_type(4) -> entrance;
area_type(5) -> building;
area_type(Level) -> Level.

find_area(Id) ->
    find_area(lookup(id,Id), []).

find_area(false, Acc) ->
    Acc;
find_area(#area{type=city} = Area, Acc) ->
    [Area|Acc];
find_area(#area{parent_id=PId} = Area, Acc) ->
    find_area(lookup(PId), [Area|Acc]).

find_area_id(Id) ->
    [{Area#area.type, Area#area.id} || Area <- find_area(Id)].
