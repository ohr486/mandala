-module(mandala_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

%%--------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, ok).

%%--------------------------------------------------------------------
init(ok) ->
  Workers = [
    {
      mandala_config,
      {mandala_config, start_link, []},

      permanent,       % Restart
      2000,            % Shutdown
      worker,          % Type
      [mandala_config] % Modules
    },

    {
      mandala_code_server,
      {mandala_code_server, start_link, []},

      permanent,           % Restart
      2000,                % Shutdown
      worker,              % Type
      [mandala_code_server] % Modules
    }
  ],

  {ok, {{one_for_one, 3, 10}, Workers}}.
