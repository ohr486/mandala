#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname factorial -mnesia debug verbose

eval(File) ->
  {ok, B} = file:read_file(File),
  Forms = scan(erl_scan:tokens([],binary_to_list(B),1),[]),
  F = fun(X) -> {ok,Y} = erl_parse:parse_form(X), Y end,
  [F(X) || X <- Forms].

scan({done,{ok,T,N},S},Res) ->
  scan(erl_scan:tokens([],S,N),[T|Res]);
scan(_,Res) ->
  lists:reverse(Res).

main([File]) ->
  erlang:display(eval(File)).
