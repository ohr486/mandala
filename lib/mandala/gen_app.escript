#!/usr/bin/env escript
%% -*- erlang -*-

main([Source, Target, Version]) ->
  % io:format("Source:~p~nTarget:~p~nVersion:~p~n", [Source, Target, Version]),

  {ok, [{application, Name, Props0}]} = file:consult(Source),
  % io:format("Name: ~p~n", [Name]),
  % io:format("Props0: ~p~n", [Props0]),

  Ebin = filename:dirname(Target),
  Files = filelib:wildcard(filename:join(Ebin, "*.beam")),
  Mods = [list_to_atom(filename:basename(F, ".beam")) || F <- Files],
  Props1 = lists:keyreplace(modules, 1, Props0, {modules, Mods}),
  % io:format("Props1: ~p~n", [Props1]),

  Props = lists:keyreplace(vsn, 1, Props1, {vsn, Version}),
  % io:format("Props: ~p~n", [Props]),

  App = io_lib:format("~tp.~n", [{application, Name, Props}]),
  ok = file:write_file(Target, App),
  io:format("==> Generated: ~ts~n", [Target]).

