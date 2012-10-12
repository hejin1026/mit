-module(mit_app).

-created("hejin 2012-4-26").

-behavior(application).

-export([start/0,start/2,stop/1,stop/0]).

start() ->
    application:start(crypto),
    [ start_app(App) || App <- [mnesia, elog, amqp_client, emysql, iconv,worker_pool, mit] ],
	timer:sleep(1000),
    io:format("~n~s: Finished.~n", [node()]).

stop() ->
    [ stop_app(App) || App <- [mit,worker_pool, iconv, emysql, amqp_client, elog, mnesia] ],
    application:stop(crypto).


start_app(mnesia) ->
    case mnesia:system_info(extra_db_nodes) of
    [] ->
        mnesia:delete_schema([node()]),
        mnesia:create_schema([node()]);
    _ ->
        ok
    end,
    mnesia:start(),
    mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity);

start_app(App) ->
    ensure_start(App).

ensure_start(App) ->
	case application:start(App) of
		ok -> ok;
		{error, {already_started, _}} -> ok
	end.

stop_app(elog) ->
	application:stop(syntax_tools),
	application:stop(compiler),
	application:stop(elog);

stop_app(App) ->
    application:stop(App).


start(normal, _Args) ->
	mit_sup:start_link().

stop(_) ->
	ok.

