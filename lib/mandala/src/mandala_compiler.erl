-module(mandala_compiler).

%% API
-export([bootstrap/0]).

bootstrap() ->
  {ok, _} = application:ensure_all_started(mandala),
  {Init, Main} = bootstrap_files(),
  [bootstrap_file(File) || File <- [<<"lib/mandala/lib/kernel.mdl">> | Init]],
  [bootstrap_file(File) || File <- [<<"lib/mandala/lib/kernel.mdl">> | Main]].

%%%===================================================================
%%% Internal functions
%%%===================================================================

bootstrap_files() ->
  {
    [],
    []
  }.

bootstrap_file(File) ->
  try
    Lists = file(filename:absname(File), fun(_, _) -> ok end),
    _ = [binary_to_path(X, "lib/mandala/ebin") || X <- Lists],
    io:format("Compiled ~ts~n", [File])
  catch
    Kind:Reason:Stacktrace ->
      io:format("~p: ~p~nstacktrace: ~p~n", [Kind, Reason, Stacktrace]),
      erlang:halt(1)
  end.

binary_to_path({ModuleName, _ModuleMap, Binary}, CompilePath) ->
  Path = filename:join(CompilePath, atom_to_list(ModuleName) ++ ".beam"),
  case file:write_file(Path, Binary) of
    ok -> Path;
    {error, Reason} -> error('Mandala.File.Error':exception([{action, "write to"}, {path, Path}, {reason, Reason}]))
  end.

file(File, Callback) ->
  {ok, Bin} = file:read_file(File),
  string(mandala_utils:characters_to_list(Bin), File, Callback).

string(_Contents, _File, _Callback) ->
%  Forms = mandala:'string_to_quoted!'(
%    Contents, 1, 1, File, mandala_config:get(parser_options)),
%  quoted(Forms, File, Callback).
  [].

