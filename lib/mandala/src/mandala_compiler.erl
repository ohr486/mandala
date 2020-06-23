-module(mandala_compiler).
-include("mandala.hrl").

%% API
-export([bootstrap/0]).

bootstrap() ->
  {ok, _} = application:ensure_all_started(mandala),
  mandala_config:put(parser_options, []),
  {Init, Main} = bootstrap_files(),
  [bootstrap_file(File) || File <- [<<"lib/mandala/lib/kernel.mdl">> | Init]],
  [bootstrap_file(File) || File <- [<<"lib/mandala/lib/kernel.mdl">> | Main]].

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
bootstrap_files() ->
  {
    [],
    []
  }.

%%--------------------------------------------------------------------
bootstrap_file(File) ->
  try
    Lists = file(filename:absname(File), fun(_, _) -> ok end),
    _ = [binary_to_path(X, "lib/mandala/ebin") || X <- Lists],
    io:format("Compiled: ~ts~n", [File])
  catch
    Kind:Reason:Stacktrace ->
      io:format("~p: ~p~nstacktrace: ~p~n", [Kind, Reason, Stacktrace]),
      erlang:halt(1)
  end.

%%--------------------------------------------------------------------
binary_to_path({ModuleName, _ModuleMap, Binary}, CompilePath) ->
  Path = filename:join(CompilePath, atom_to_list(ModuleName) ++ ".beam"),
  case file:write_file(Path, Binary) of
    ok -> Path;
    {error, Reason} -> error('Mandala.File.Error':exception([{action, "write to"}, {path, Path}, {reason, Reason}]))
  end.

%%--------------------------------------------------------------------
file(File, Callback) ->
  {ok, Bin} = file:read_file(File),
  string(mandala_utils:characters_to_list(Bin), File, Callback).

%%--------------------------------------------------------------------
string(Contents, File, Callback) ->
  Forms = mandala:'string_to_quoted!'(
    Contents, 1, 1, File, mandala_config:get(parser_options)),
  quoted(Forms, File, Callback).

%%--------------------------------------------------------------------
quoted(Forms, File, Callback) ->
  Previous = get(mandala_module_binaries),

  try
    put(mandala_module_binaries, []),
    Env = (mandala_env:new())#{
      line := 1,
      file := File,
      tracers := mandala_config:get(tracers)},

    mandala_lexical:run(
      Env,
      fun (LexicalEnv) -> eval_forms(Forms, [], LexicalEnv) end,
      fun (#{lexical_tracker := Pid}) -> Callback(File, Pid) end
    ),

    lists:reverse(get(mandala_module_binaries))
  after
    put(mandala_module_binaries, Previous)
  end.

%%--------------------------------------------------------------------
eval_forms(Forms, Args, E) ->
  case (?key(E, module) == nil) andalso allows_fast_compilation(Forms) of
    true  ->
      {Result, _Binding, EE} = mandala:eval_forms(Forms, [], E),
      {Result, EE};
    false ->
      compile(Forms, Args, E)
  end.

%%--------------------------------------------------------------------
retrieve_compiler_module() ->
  mandala_code_server:call(retrieve_compiler_module).

%%--------------------------------------------------------------------
return_compiler_module(Module, Purgeable) ->
  mandala_code_server:cast({return_compiler_module, Module, Purgeable}).

%%--------------------------------------------------------------------
is_purgeable(Module, Binary) ->
  beam_lib:chunks(Binary, [labeled_locals])
    == {ok, {Module, [{labeled_locals, []}]}}.

%%--------------------------------------------------------------------
allows_fast_compilation({'__block__', _, Exprs}) ->
  lists:all(fun allows_fast_compilation/1, Exprs);
allows_fast_compilation({defmodule, _, _}) ->
  true;
allows_fast_compilation(_) ->
  false.

%%--------------------------------------------------------------------
compile(Quoted, ArgsList, E) ->
  Args = list_to_tuple(ArgsList),
  {Expanded, EE} = mandala_expand:expand(Quoted, E),
  mandala_env:check_unused_vars(EE),

  {Module, Fun, Purgeable} =
    mandala_erl_compiler:spawn(fun spawned_compile/2, [Expanded, E]),

  {dispatch(Module, Fun, Args, Purgeable), EE}.

%%--------------------------------------------------------------------
spawned_compile(ExExprs, #{line := Line, file := File} = E) ->
  {Vars, S} = mandala_env:env_to_scope(E),
  {ErlExprs, _} = mandala_erl_pass:translate(ExExprs, S),

  Module = retrieve_compiler_module(),
  Fun  = code_fun(?key(E, module)),
  Forms = code_mod(Fun, ErlExprs, Line, File, Module, Vars),

  {Module, Binary} = mandala_erl_compiler:noenv_forms(Forms, File, [nowarn_nomatch]),
  code:load_binary(Module, "", Binary),
  {Module, Fun, is_purgeable(Module, Binary)}.

%%--------------------------------------------------------------------
dispatch(Module, Fun, Args, Purgeable) ->
  Res = Module:Fun(Args),
  code:delete(Module),
  Purgeable andalso code:purge(Module),
  return_compiler_module(Module, Purgeable),
  Res.

%%--------------------------------------------------------------------
code_fun(nil) -> '__FILE__';
code_fun(_)   -> '__MODULE__'.

%%--------------------------------------------------------------------
code_mod(Fun, Expr, Line, File, Module, Vars)
  when is_binary(File), is_integer(Line) ->
  Ann = erl_anno:new(Line),
  Tuple = {tuple, Ann, [{var, Ann, Var} || {_, Var} <- Vars]},
  Relative = mandala_utils:relative_to_cwd(File),

  [{attribute, Ann, file, {mandala_utils:characters_to_list(Relative), 1}},
   {attribute, Ann, module, Module},
   {attribute, Ann, compile, no_auto_import},
   {attribute, Ann, export, [{Fun, 1}, {'__RELATIVE__', 0}]},
   {function, Ann, Fun, 1, [
     {clause, Ann, [Tuple], [], [Expr]}
   ]},
   {function, Ann, '__RELATIVE__', 0, [
     {clause, Ann, [], [], [mandala_erl:mandala_to_erl(Relative)]}
   ]}].

%%--------------------------------------------------------------------
