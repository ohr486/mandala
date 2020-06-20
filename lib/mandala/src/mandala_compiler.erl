-module(mandala_compiler).

%% API
-export([bootstrap/0]).

bootstrap() ->
  {ok, _} = application:ensure_all_started(mandala),
  ok.