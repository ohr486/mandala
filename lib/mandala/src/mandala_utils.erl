-module(mandala_utils).

%% API
-export([characters_to_list/1, characters_to_binary/1]).

%%--------------------------------------------------------------------
characters_to_list(Data) when is_list(Data) ->
  Data;
characters_to_list(Data) ->
  case unicode:characters_to_list(Data) of
    Result when is_list(Result) -> Result;
    {error, Encoded, Rest} -> conversion_error(invalid, Encoded, Rest);
    {incomplete, Encoded, Rest} -> conversion_error(incomplete, Encoded, Rest)
  end.

%%--------------------------------------------------------------------
characters_to_binary(Data) when is_binary(Data) ->
  Data;
characters_to_binary(Data) ->
  case unicode:characters_to_binary(Data) of
    Result when is_binary(Result) -> Result;
    {error, Encoded, Rest} -> conversion_error(invalid, Encoded, Rest);
    {incomplete, Encoded, Rest} -> conversion_error(incomplete, Encoded, Rest)
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

conversion_error(Kind, Encoded, Rest) ->
  error('Mandala.UnicodeConversionError':exception(
    [{encoded, Encoded}, {rest, Rest}, {kind, Kind}])).
