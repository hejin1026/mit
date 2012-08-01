-module(mit_area).

-author('chibj.opengoss@gmail.com').

-behavior(gen_server).

-export([start_link/0, stop/0]).

-export([lookup/1]).

-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-include("mit.hrl").
-include_lib("elog/include/elog.hrl").

-record(area, {dn,id,name,data}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec () -> ok
%% @doc Stop the mit server.
stop() ->
	gen_server:cast(?MODULE, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%% {ok, State, Timeout} |
%% ignore |
%% {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    case mnesia:system_info(extra_db_nodes) of
        [] -> %master node
            {atomic, ok} = mnesia:create_table(areas,
                [{ram_copies, [node()]},
                 {attributes, record_info(fields, area)}]),
				self() ! load_areas,
		    erlang:send_after(30*60*1000, self(), load_areas),
            io:format("finish start mit areas...~n",[]);
        _ -> %slave node
            ok
    end,
    {ok, state}.



%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%% {reply, Reply, State, Timeout} |
%% {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, Reply, State} |
%% {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call(Req, _From, State) ->
?ERROR("Unexpected requrest: ~p", [Req]),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
{stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(load_areas, State) ->
    case emysql:select(areas) of
        {ok, Areas} ->
		 	?INFO("areas ~p",[Areas]);
            lists:foreach(fun(Area) ->
			        {value, Dn} = dataset:get_value(area_dn, Area),
			        {value, Id} = dataset:get_value(id, Area),
			        {value, Name} = dataset:get_value(area_name, Area),
		           mnesia:sync_dirty(fun() ->
		                mnesia:write(#area{dn=Dn, id=Id, name=Name,data=Area})
		            end)
			    end, Areas),
			 	?WARNING("normal load areas   ~p",[length(Areas)]);
        {error, Reason} ->
            ?ERROR("load areas failure...~p",[Reason])
		end,
    erlang:send_after(1440000, self(), load_areas),
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

lookup(Dn) ->
   	lookup(dn,Dn).

lookup(dn,Dn) ->
   	case mnesia:dirty_read(areas, Dn) of
        [Area] ->
            Area#area.data;
        [] ->
            []
    end;
lookup(id,Id) ->
   case mnesia:dirty_index_read(areas, Id, #area.id) of
    [Area] ->
        Area#area.data;
    [_Area|_] ->
        false;
    [] ->
        false
    end.

%interface for evabus_filter
find_area_path(Id) ->
	case lookup(id,Id) of
		false ->[];
		Area ->
			{value, ParentId} = dataset:get_value(parent_id, Area),
			{value, Level} = dataset:get_value(area_level, Area),
			case Level of
				4 -> find_area_info(branchid,BranchId,[{entranceid,Id}]);
				3 -> [{branchid,Id},{townid,ParentId}];
				_ -> []
	end.

find_area_info(branchid,Id,Acc) ->
	case lookup(id,Id) of
		false ->Acc;
		Area->
			{value, TownId} = dataset:get_value(parent_id, Area),
			Acc++[{townId,TownId},{branchid,Id}]
	end.














