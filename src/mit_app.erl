-module(mit_app).

-created("hejin 2012-4-26").

-behavior(application).

-export([start/0, stop/0]).

-export([start/2, stop/1]).

start() ->
    application:start(mit).

stop() ->
    application:stop(mit).


start(normal, _Args) ->
	{ok, SupPid} = mit_sup:start_link(),
	{ok, SupPid}.	

stop(_) ->
	ok.

