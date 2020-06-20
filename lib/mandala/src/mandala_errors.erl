-module(mandala_errors).

%% API
-export([erl_warn/3, parse_error/4]).

%%--------------------------------------------------------------------
-spec erl_warn(non_neg_integer() | none, unicode:chardata(), unicode:chardata())
      -> ok.
erl_warn(none, File, Warning) ->
  erl_warn(0, File, Warning);
erl_warn(Line, File, _Warning) when is_integer(Line), is_binary(File) ->
%  io_warn(Line, File, Warning, [Warning, "\n  ", file_format(Line, File), $\n]).
  ok.

%%--------------------------------------------------------------------
-spec parse_error(non_neg_integer(), binary() | {binary(), binary()},
    binary(), binary()) -> no_return().
parse_error(Line, File, Error, <<>>) ->
  Message = case Error of
              <<"syntax error before: ">> -> <<"syntax error: expression is incomplete">>;
              _ -> Error
            end,
  %raise(Line, File, 'Mandala.TokenMissingError', Message).
  erlang:raise(Line, File, 'Mandala.TokenMissingError', Message).
