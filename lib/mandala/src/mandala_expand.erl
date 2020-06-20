-module(mandala_expand).
-include("mandala.hrl").
-import(mandala_errors, [form_error/4]).

%% API
-export([expand/2]).

%%--------------------------------------------------------------------
expand({'=', Meta, [Left, Right]}, E) ->
  assert_no_guard_scope(Meta, "=", E),
  {ERight, ER} = expand(Right, E),
  {ELeft, EL} = mandala_clauses:match(fun expand/2, Left, ER, E),
  refute_parallel_bitstring_match(ELeft, ERight, E, ?key(E, context) == match),
  {{'=', Meta, [ELeft, ERight]}, EL};

expand({_, _, _} = Tuple, E) ->
  form_error([{line, 0}], ?key(E, file), ?MODULE, {invalid_quoted_expr, Tuple});

expand(Other, E) ->
  form_error([{line, 0}], ?key(E, file), ?MODULE, {invalid_quoted_expr, Other}).

%%--------------------------------------------------------------------
assert_no_guard_scope(Meta, Kind, #{context := guard, file := File}) ->
  form_error(Meta, File, ?MODULE, {invalid_expr_in_guard, Kind});
assert_no_guard_scope(_Meta, _Kind, _E) -> [].

%%--------------------------------------------------------------------
refute_parallel_bitstring_match(_Left, _Right, _E, _Parallel) ->
  ok.
