-module(mandala).
-behaviour(application).

%% Application callbacks
-export([start/2,
  stop/1]).

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

%%%===================================================================
%%% Internal functions
%%%===================================================================
