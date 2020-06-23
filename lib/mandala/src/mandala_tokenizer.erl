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
    mandala_config:get(identifier_tokenizer, 'Mandala.String.Tokenizer'),
  Scope = [],
  tokenize(String, Line, Column, Scope, []).

%%--------------------------------------------------------------------
tokenize(String, Line, Opts) ->
  tokenize(String, Line, 1, Opts).

%%--------------------------------------------------------------------
tokenize(
    [], _Line, _Column,
    #mandala_tokenizer{terminators=[], warnings = Warnings},
    Tokens
) ->
  [mandala_errors:erl_warn(Line, File, Msg) ||
    {Line, File, Msg} <- lists:reverse(Warnings)],
  {ok, lists:reverse(Tokens)};

tokenize(
    [], EndLine, Column,
    Scope, Tokens
) ->
  #mandala_tokenizer{terminators=[{Start, StartLine, _} | _]} = Scope,
  End = terminator(Start),
  Hint = missing_terminator_hint(Start, End, Scope),

  Message =
    io_lib:format("missing terminator: ~ts (for \"~ts\" starting at line ~B)", [End, Start, StartLine]),

  {error, {EndLine, Column, [Message, Hint], []}, [], Tokens}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


missing_terminator_hint(Start, End, #mandala_tokenizer{mismatch_hints=Hints}) ->
  case lists:keyfind(Start, 1, Hints) of
    {Start, HintLine, _} ->
      io_lib:format("\n\n    HINT: it looks like the \"~ts\" on line ~B does not have a matching \"~ts\"\n",
        [Start, HintLine, End]);
    false ->
      ""
  end.

terminator('fn') -> 'end';
terminator('do') -> 'end';
terminator('(')  -> ')';
terminator('[')  -> ']';
terminator('{')  -> '}';
terminator('<<') -> '>>'.
