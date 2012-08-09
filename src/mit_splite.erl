-module(mit_splite).

-author("hejin 2011-3-3").

-include("mit.hrl").
-include_lib("elog/include/elog.hrl").

%api
-export([lookup/1,add/2]).

-import(extbif, [to_list/1, to_binary/1]).


lookup(Dn) ->
    case mit:lookup(Dn) of
    {ok, #entry{data = Splite, type = splite}} ->
        {ok, Splite};
    false ->
        false;
    _Other ->
        false
    end.


add(Dn, Splite) ->
     insert_splite(Dn, Splite).

insert_splite(PonDn,Splite)->
	?INFO(" insert_splite: ~p ~p", [PonDn, Splite]),
     case emysql:insert(mit_splites, Splite) of
            {updated, {0, _}} ->
                  ?WARNING("cannot inserted Splite: ~p ~p", [PonDn, Splite]);
            {updated, {1, _PId}} ->
                  ok;
            {error, Reason} ->
                  ?WARNING("~p", [Reason])
      end.
