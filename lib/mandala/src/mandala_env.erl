-module(mandala_env).
-include("mandala.hrl").

%% API
-export([new/0]).

new() ->
  #{
    '__struct__' => 'Mandala.Macro.Env',
    module => nil,                                     %% the current module
    file => <<"nofile">>,                              %% the current filename
    line => 1,                                         %% the current line
    function => nil,                                   %% the current function
    context => nil,                                    %% can be match, guard or nil
    aliases => [],                                     %% a list of aliases by new -> old names
    requires => mandala_dispatch:default_requires(),   %% a set with modules required
    functions => mandala_dispatch:default_functions(), %% a list with functions imported from module
    macros => mandala_dispatch:default_macros(),       %% a list with macros imported from module
    macro_aliases => [],                               %% keep aliases defined inside a macro
    context_modules => [],                             %% modules defined in the current context
    vars => [],                                        %% a set of defined variables
    current_vars => {#{}, false},                      %% a tuple with maps of read and optional write current vars
    unused_vars => {#{}, 0},                           %% a map of unused vars and a version counter for vars
    prematch_vars => warn,                             %% controls behaviour outside and inside matches
    lexical_tracker => nil,                            %% lexical tracker PID
    contextual_vars => [],                             %% available contextual variables
    tracers => []                                      %% available compilation tracers
  }.
