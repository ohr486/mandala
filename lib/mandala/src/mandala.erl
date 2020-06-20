-module(mandala).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, 'string_to_quoted!'/5, eval_forms/3]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {ok, pid(), State :: term()} |
  {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
  Tab = mandala_config:new([]),
  case mandala_sup:start_link() of
    {ok, Sup} ->
      {ok, Sup, Tab};
    {error, _Reason} = Error ->
      mandala_config:delete(Tab),
      Error
  end.

%%--------------------------------------------------------------------
-spec(stop(Tab :: term()) -> term()).
stop(Tab) ->
  mandala_config:delete(Tab).

%%--------------------------------------------------------------------
'string_to_quoted!'(String, StartLine, StartColumn, File, Opts) ->
  case string_to_tokens(String, StartLine, StartColumn, File, Opts) of
    {ok, Tokens} ->
      case tokens_to_quoted(Tokens, File, Opts) of
        {ok, Forms} ->
          Forms;
        {error, {Line, Error, Token}} ->
          mandala_errors:parse_error(Line, File, Error, Token)
      end;
    {error, {Line, Error, Token}} ->
      mandala_errors:parse_error(Line, File, Error, Token)
  end.

%%--------------------------------------------------------------------
string_to_tokens(String, StartLine, StartColumn, File, Opts)
  when is_integer(StartLine), is_binary(File) ->
  case mandala_tokenizer:tokenize(
    String, StartLine, StartColumn, [{file, File} | Opts]
  ) of
    {ok, _Tokens} = Ok ->
      Ok;
    {error, {Line, _, {ErrorPrefix, ErrorSuffix}, Token}, _Rest, _SoFar} ->
      {error, {
        Line,
        {to_binary(ErrorPrefix), to_binary(ErrorSuffix)},
        to_binary(Token)}};
    {error, {Line, _, Error, Token}, _Rest, _SoFar} ->
      {error, {
        Line,
        to_binary(Error),
        to_binary(Token)}}
  end.

%%--------------------------------------------------------------------
to_binary(List) when is_list(List) -> mandala_utils:characters_to_binary(List);
to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8).

%%--------------------------------------------------------------------
tokens_to_quoted(Tokens, File, Opts) ->
  handle_parsing_opts(File, Opts),

  try mandala_parser:parse(Tokens) of
    {ok, Forms} ->
      {ok, Forms};
    {error, {Line, _, [{ErrorPrefix, ErrorSuffix}, Token]}} ->
      {error, {
        parser_line(Line),
        {to_binary(ErrorPrefix), to_binary(ErrorSuffix)},
        to_binary(Token)}};
    {error, {Line, _, [Error, Token]}} ->
      {error, {
        parser_line(Line),
        to_binary(Error),
        to_binary(Token)}}
  after
    erase(mandala_parser_file),
    erase(mandala_parser_columns),
    erase(mandala_token_metadata),
    erase(mandala_literal_encoder)
  end.

%%--------------------------------------------------------------------
handle_parsing_opts(_File, _Opts) ->
  ok.

%%--------------------------------------------------------------------
parser_line({Line, _, _}) ->
  Line;
parser_line(Meta) ->
  case lists:keyfind(line, 1, Meta) of
    {line, L} -> L;
    false -> 0
  end.

%%--------------------------------------------------------------------
eval_forms(Tree, Binding, Opts) when is_list(Opts) ->
  eval_forms(Tree, Binding, env_for_eval(Opts));
eval_forms(Tree, RawBinding, OE) ->
  {Vars, Binding} = normalize_binding(RawBinding, [], []),
  E = elixir_env:with_vars(OE, Vars),
  {_, S} = elixir_env:env_to_scope(E),
  {Erl, NewE, NewS} = quoted_to_erl(Tree, E, S),

  case Erl of
    {atom, _, Atom} ->
      {Atom, Binding, NewE};

    _  ->
      Exprs =
        case Erl of
          {block, _, BlockExprs} -> BlockExprs;
          _ -> [Erl]
        end,

      ErlBinding = elixir_erl_var:load_binding(Binding, E, S),
      {value, Value, NewBinding} = recur_eval(Exprs, ErlBinding, NewE),
      {Value, elixir_erl_var:dump_binding(NewBinding, NewE, NewS), NewE}
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
