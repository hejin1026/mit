-module(mit_app).

-created("hejin 2012-4-26").

-behavior(application).

-export([start/2, stop/1]).

start(normal, _Args) ->
	mit_sup:start_link().

stop(_) ->
	ok.

