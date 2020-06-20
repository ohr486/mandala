-module(mandala_errors).
-include("mandala.hrl").

%% API
-export([erl_warn/3, parse_error/4, form_error/4]).

%%--------------------------------------------------------------------
erl_warn(none, File, Warning) ->
  erl_warn(0, File, Warning);
erl_warn(Line, File, _Warning) when is_integer(Line), is_binary(File) ->
%  io_warn(Line, File, Warning, [Warning, "\n  ", file_format(Line, File), $\n]).
  ok.

%%--------------------------------------------------------------------
parse_error(Line, File, Error, <<>>) ->
  Message = case Error of
              <<"syntax error before: ">> -> <<"syntax error: expression is incomplete">>;
              _ -> Error
            end,
  %raise(Line, File, 'Mandala.TokenMissingError', Message).
  erlang:raise(Line, File, 'Mandala.TokenMissingError', Message).

%%--------------------------------------------------------------------
form_error(Meta, #{file := File}, Module, Desc) ->
  compile_error(Meta, File, Module:format_error(Desc));
form_error(Meta, File, Module, Desc) ->
  compile_error(Meta, File, Module:format_error(Desc)).

%%--------------------------------------------------------------------
compile_error(Meta, File, Message) when is_binary(Message) ->
  {MetaFile, MetaLine} = meta_location(Meta, File),
  raise(MetaLine, MetaFile, 'Mandala.CompileError', Message);
compile_error(Meta, File, Message) when is_list(Message) ->
  {MetaFile, MetaLine} = meta_location(Meta, File),
  raise(
    MetaLine, MetaFile, 'Mandala.CompileError',
    mandala_utils:characters_to_binary(Message)).

%%--------------------------------------------------------------------
meta_location(Meta, File) ->
  case mandala_utils:meta_keep(Meta) of
    {F, L} -> {F, L};
    nil    -> {File, ?line(Meta)}
  end.

%%--------------------------------------------------------------------
raise(none, File, Kind, Message) ->
  raise(0, File, Kind, Message);
raise({Line, _, _}, File, Kind, Message) when is_integer(Line) ->
  raise(Line, File, Kind, Message);
raise(Line, File, Kind, Message)
  when is_integer(Line), is_binary(File), is_binary(Message) ->
  Stacktrace = try throw(ok) catch _:_:Stack -> Stack end,
  Exception = Kind:exception([
    {description, Message},
    {file, File},
    {line, Line}]),
  erlang:raise(error, Exception, tl(Stacktrace)).
