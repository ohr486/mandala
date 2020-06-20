-module(mandala_tokenizer).
-include("mandala.hrl").

%% API
-export([tokenize/4]).

%%--------------------------------------------------------------------
tokenize(
    String, Line, Column,
    #mandala_tokenizer{} = Scope
) ->
  tokenize(String, Line, Column, Scope, []);

tokenize(String, Line, Column, _Opts) ->
  _IdentifierTokenizer =
    elixir_config:get(identifier_tokenizer, 'Mandala.String.Tokenizer'),
  Scope = [],
  tokenize(String, Line, Column, Scope, []).

tokenize(
    [], _Line, _Column,
    #mandala_tokenizer{terminators=[], warnings = Warnings},
    Tokens
) ->
  [mandala_errors:erl_warn(Line, File, Msg) ||
    {Line, File, Msg} <- lists:reverse(Warnings)],
  {ok, lists:reverse(Tokens)}.