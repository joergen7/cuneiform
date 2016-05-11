%% -*- erlang -*-
%
% Cuneiform: A Functional Language for Large Scale Scientific Data Analysis
%
% Copyright 2016 Jörgen Brandt, Marc Bux, and Ulf Leser
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

-module( cf_sem_test ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).

-include_lib( "eunit/include/eunit.hrl" ).

-define( THETA0, {#{}, fun mu/1, #{}, #{}} ).

mu( {app, _AppLine, _C, {lam, _LamLine, LamName, {sign, Lo, _Li}, _B}, _Fa} ) ->
  {fut, LamName, random:uniform( 1000000000 ), Lo}.

nil_should_eval_itself_test() ->
  ?assertEqual( [], cf_sem:eval( [], ?THETA0 ) ).

str_should_eval_itself_test() ->
  E = [{str, "bla"}],
  ?assertEqual( E, cf_sem:eval( E, ?THETA0 ) ).

undef_var_should_fail_test() ->
  E = [{var, 1, "x"}],
  ?assertThrow( {1, cf_sem, _}, cf_sem:eval( E, ?THETA0 ) ).

def_var_should_eval_to_bound_value_test() ->
  E = [{str, "blub"}],
  X = cf_sem:eval( [{var, 2, "x"}], {#{"x" => E}, fun mu/1, #{}, #{}} ),
  ?assertEqual( E, X ).

def_var_should_cascade_binding_test() ->
  E = [{str, "blub"}],
  Theta = {#{"x" => [{var, 2, "y"}], "y" => E}, fun mu/1, #{}, #{}},
  X = cf_sem:eval( [{var, 3, "x"}], Theta ),
  ?assertEqual( E, X ).

def_var_should_cascade_binding_twice_test() ->
  A = [{str, "A"}],
  Rho = #{"x" => [{var, 2, "y"}], "y" => [{var, 3, "z"}], "z" => A},
  ?assertEqual( A, cf_sem:eval( [{var, 4, "x"}], {Rho, fun mu/1, #{}, #{}} ) ).

unfinished_fut_should_eval_to_itself_test() ->
  Fut = {fut, "f", 1234, [{param, {name, "out", false}, false}]},
  E = [{select, 2, 1, Fut}],
  X = cf_sem:eval( E, ?THETA0 ),
  ?assertEqual( E, X ).


finished_fut_should_eval_to_result_test() ->
  Fut = {fut, "f", 1234, [{param, {name, "out", false}, false}]},
  S = {select, 2, 1, Fut},
  F = [{str, "blub"}],
  Theta = {#{}, fun mu/1, #{}, #{{"out", 1234} => F}},
  X = cf_sem:eval( [S], Theta ),
  ?assertEqual( F, X ).

noarg_fn_should_eval_plain_test() ->
  E = [{str, "bla"}],
  Sign = {sign, [{param, {name, "out", false}, false}], []},
  Body = {natbody, #{"out" => E}},
  Lam = {lam, 2, "f", Sign, Body},
  F = [{app, 3, 1, Lam, #{}}],
  ?assertEqual( E, cf_sem:eval( F, ?THETA0 ) ).

noarg_fn_should_eval_body_test() ->
  E = [{str, "bla"}],
  Sign = {sign, [{param, {name, "out", false}, false}], []},
  Body = {natbody, #{"out" => [{var, 2, "x"}], "x" => E}},
  Lam = {lam, 3, "f", Sign, Body},
  F = [{app, 4, 1, Lam, #{}}],
  ?assertEqual( E, cf_sem:eval( F, ?THETA0 ) ).

fn_call_should_insert_lam_test() ->
  E = [{str, "bla"}],
  Sign = {sign, [{param, {name, "out", false}, false}], []},
  Body = {natbody, #{"out" => E}},
  Lam = {lam, 2, "f", Sign, Body},
  F = [{app, 3, 1, {var, 4, "f"}, #{}}],
  Theta = {#{}, fun mu/1, #{"f" => Lam}, #{}},
  ?assertEqual( E, cf_sem:eval( F, Theta ) ).

app_with_unbound_lam_should_fail_test() ->
  F = [{app, 1, 1, {var, 2, "f"}, #{}}],
  ?assertThrow( {1, cf_sem, _}, cf_sem:eval( F, ?THETA0 ) ).

identity_fn_should_eval_arg_test() ->
  E = [{str, "bla"}],
  Sign = {sign, [{param, {name, "out", false}, false}], [{param, {name, "inp", false}, false}]},
  Body = {natbody, #{"out" => [{var, 2, "inp"}]}},
  Lam = {lam, 3, "f", Sign, Body},
  F = [{app, 4, 1, Lam, #{"inp" => E}}],
  ?assertEqual( E, cf_sem:eval( F, ?THETA0 ) ).

multiple_output_should_be_bindable_test() ->
  Sign = {sign, [{param, {name, "out1", false}, false}, {param, {name, "out2", false}, false}], []},
  E1 = [{str, "bla"}],
  E2 = [{str, "blub"}],
  Body = {natbody, #{"out1" => E1, "out2" => E2}},
  Lam = {lam, 3, "f", Sign, Body},
  F1 = [{app, 4, 1, Lam, #{}}],
  F2 = [{app, 5, 2, Lam, #{}}],
  [?assertEqual( E1, cf_sem:eval( F1, ?THETA0 ) ),
   ?assertEqual( E2, cf_sem:eval( F2, ?THETA0 ) )].

app_should_ignore_calling_context_test() ->
  Sign = {sign, [{param, {name, "out", false}, false}], []},
  Body = {natbody, #{"out" => [{var, 1, "x"}]}},
  Lam = {lam, 2, "f", Sign, Body},
  X = [{app, 3, 1, Lam, #{}}],
  Rho = #{"x" => [{str, "blub"}]},
  ?assertThrow( {1, cf_sem, _}, cf_sem:eval( X, {Rho, fun mu/1, #{}, #{}} ) ).

app_should_hand_down_gamma_test() ->
  Sign = {sign, [{param, {name, "out", false}, false}], []},
  Body = {natbody, #{"out" => [{app, 1, 1, {var, 2, "f"}, #{}}]}},
  Lam = {lam, 3, "g", Sign, Body},
  X = [{app, 4, 1, Lam, #{}}],
  E = [{str, "blub"}],
  Gamma = #{"f" => {lam, 6, "f", Sign, {natbody, #{"out" => E}}}},
  Theta = {#{}, fun mu/1, Gamma, #{}},
  ?assertEqual( E, cf_sem:eval( X, Theta ) ).

binding_should_override_body_test() ->
  F = [{str, "blub"}],
  Sign = {sign, [{param, {name, "out", false}, false}], [{param, {name, "x", false}, false}]},
  Body = {natbody, #{"x" => [{str, "bla"}], "out" => [{var, 3, "x"}]}},
  Lam = {lam, 4, "f", Sign, Body},
  X = [{app, 5, 1, Lam, #{"x" => F}}],
  ?assertEqual( F, cf_sem:eval( X, ?THETA0 ) ).
  
returning_empty_list_on_nonlist_output_channel_should_fail_test() ->
  S = {sign, [{param, {name, "out", false}, false}], []},
  B = {natbody, #{"out" => []}},
  Lam = {lam, 1, "f", S, B},
  X = [{app, 2, 1, Lam, #{}}],
  ?assertError( output_sign_mismatch, cf_sem:eval( X, ?THETA0 ) ).

cross_product_should_be_derivable_test() ->
  Sign = {sign, [{param, {name, "out1", false}, false}, {param, {name, "out2", false}, false}],
                [{param, {name, "p1", false}, false}, {param, {name, "p2", false}, false}]},
  E1 = [{str, "A"}, {str, "B"}],
  E2 = [{str, "1"}, {str, "2"}],
  Body = {natbody, #{"out1" => [{var, 5, "p1"}], "out2" => [{var, 6, "p2"}]}},
  Lam = {lam, 7, "f", Sign, Body},
  Binding = #{"p1" => E1, "p2" => E2},
  App1 = [{app, 8, 1, Lam, Binding}],
  App2 = [{app, 9, 2, Lam, Binding}],
  F1 = [{str, "A"}, {str, "A"}, {str, "B"}, {str, "B"}],
  F2 = [{str, "1"}, {str, "2"}, {str, "1"}, {str, "2"}],
  [?assertEqual( F1, cf_sem:eval( App1, ?THETA0 ) ),
   ?assertEqual( F2, cf_sem:eval( App2, ?THETA0 ) )].

dot_product_should_be_derivable1_test() ->
  Sign = {sign, [{param, {name, "out1", false}, false}, {param, {name, "out2", false}, false}],
                [{correl, [{name, "p1", false}, {name, "p2", false}]}]},
  E1 = [{str, "A"}],
  E2 = [{str, "1"}],
  Body = {natbody, #{"out1" => [{var, 5, "p1"}], "out2" => [{var, 6, "p2"}]}},
  Lam = {lam, 7, "f", Sign, Body},
  Binding = #{"p1" => E1, "p2" => E2},
  App1 = [{app, 8, 1, Lam, Binding}],
  App2 = [{app, 9, 2, Lam, Binding}],
  [?assertEqual( E1, cf_sem:eval( App1, ?THETA0 ) ),
   ?assertEqual( E2, cf_sem:eval( App2, ?THETA0 ) )].

dot_product_should_be_derivable2_test() ->
  Sign = {sign, [{param, {name, "out1", false}, false}, {param, {name, "out2", false}, false}],
                [{correl, [{name, "p1", false}, {name, "p2", false}]}]},
  E1 = [{str, "A"}, {str, "B"}],
  E2 = [{str, "1"}, {str, "2"}],
  Body = {natbody, #{"out1" => [{var, 5, "p1"}], "out2" => [{var, 6, "p2"}]}},
  Lam = {lam, 7, "f", Sign, Body},
  Binding = #{"p1" => E1, "p2" => E2},
  App1 = [{app, 8, 1, Lam, Binding}],
  App2 = [{app, 9, 2, Lam, Binding}],
  [?assertEqual( E1, cf_sem:eval( App1, ?THETA0 ) ),
   ?assertEqual( E2, cf_sem:eval( App2, ?THETA0 ) )].

dot_product_should_be_derivable3_test() ->
  Sign = {sign, [{param, {name, "out1", false}, false}, {param, {name, "out2", false}, false}],
                [{correl, [{name, "p1", false}, {name, "p2", false}]}]},
  E1 = [{str, "A"}, {str, "B"}, {str, "C"}],
  E2 = [{str, "1"}, {str, "2"}, {str, "3"}],
  Body = {natbody, #{"out1" => [{var, 5, "p1"}], "out2" => [{var, 6, "p2"}]}},
  Lam = {lam, 7, "f", Sign, Body},
  Binding = #{"p1" => E1, "p2" => E2},
  App1 = [{app, 8, 1, Lam, Binding}],
  App2 = [{app, 9, 2, Lam, Binding}],
  [?assertEqual( E1, cf_sem:eval( App1, ?THETA0 ) ),
   ?assertEqual( E2, cf_sem:eval( App2, ?THETA0 ) )].

aggregate_should_consume_whole_list_test() ->
  Sign = {sign, [{param, {name, "out", false}, true}],
                [{param, {name, "inp", false}, true}]},
  E1 = [{str, "A"}],
  E2 = [{str, "B"}, {str, "C"}],
  Body = {natbody, #{"out" => E1++[{var, 4, "inp"}]}},
  Lam = {lam, 5, "f", Sign, Body},
  Binding = #{"inp" => E2},
  App = [{app, 6, 1, Lam, Binding}],
  ?assertEqual( E1++E2, cf_sem:eval( App, ?THETA0 ) ).

cnd_false_should_eval_else_expr_test() ->
  E = [{cnd, 1, [], [{str, "A"}], [{str, "B"}]}],
  ?assertEqual( [{str, "B"}], cf_sem:eval( E, ?THETA0 ) ).

cnd_evaluates_condition_before_decision1_test() ->
  Sign = {sign, [{param, {name, "out", false}, true}], []},
  Body = {natbody, #{"out" => []}},
  Lam = {lam, 1, "f", Sign, Body},
  App = [{app, 2, 1, Lam, #{}}],
  E = [{cnd, 3, App, [{str, "A"}], [{str, "B"}]}],
  ?assertEqual( [{str, "B"}], cf_sem:eval( E, ?THETA0 ) ).

cnd_evaluates_condition_before_decision2_test() ->
  Sign = {sign, [{param, {name, "out", false}, true}], []},
  Body = {natbody, #{"out" => [{str, "X"}]}},
  Lam = {lam, 2, "f", Sign, Body},
  App = [{app, 3, 1, Lam, #{}}],
  E = [{cnd, 4, App, [{str, "A"}], [{str, "B"}]}],
  ?assertEqual( [{str, "A"}], cf_sem:eval( E, ?THETA0 ) ).
  
cnd_evaluates_only_on_final_condition_test() ->
  Sign = {sign, [{param, {name, "out", false}, true}], []},
  Lam = {lam, 1, "f", Sign, {forbody, bash, "shalala"}},
  App = [{app, 2, 1, Lam, #{}}],
  A = [{var, 3, "a"}],
  B = [{var, 4, "b"}],
  E = [{cnd, 5, App, A, B}],
  Rho = #{"a" => [{str, "A"}], "b" => [{str, "B"}]},
  X = cf_sem:eval( E, {Rho, fun mu/1, #{}, #{}} ),
  ?assertMatch( [{cnd, 5, [{select, 2, 1, _}], A, B}], X ).

cnd_evaluates_then_expr_test() ->
  E = [{cnd, 1, [{str, "Z"}], [{var, 3, "x"}], [{str, "B"}]}],
  F = [{str, "A"}],
  Theta = {#{"x" => F}, fun mu/1, #{}, #{}},
  ?assertEqual( F, cf_sem:eval( E, Theta ) ).

cnd_evaluates_else_expr_test() ->
  E = [{cnd, 1, [], [{str, "B"}], [{var, 3, "x"}]}],
  F = [{str, "A"}],
  Theta = {#{"x" => F}, fun mu/1, #{}, #{}},
  ?assertEqual( F, cf_sem:eval( E, Theta ) ).

foreign_app_with_cnd_param_is_left_untouched_test() ->
  Sign = {sign, [{param, {name, "out", false}, false}], [{param, {name, "p", false}, false}]},
  Lam = {lam, 1, "f", Sign, {forbody, bash, "shalala"}},
  App1 = [{app, 2, 1, Lam, #{"p" => [{str, "A"}]}}],
  E = [{cnd, 4, App1, [], []}],
  App2 = [{app, 5, 1, Lam, #{"p" => E}}],
  X = cf_sem:eval( App2, ?THETA0 ),
  ?assertMatch( [{app, 5, 1, Lam, _}], X ).

foreign_app_with_select_param_is_left_untouched_test() ->
  Sign = {sign, [{param, {name, "out", false}, false}],
                [{param, {name, "p", false}, false}]},
  Lam = {lam, 1, "f", Sign, {forbody, bash, "shalala"}},
  App1 = [{app, 2, 1, Lam, #{"p" => [{str, "A"}]}}],
  App2 = [{app, 4, 1, Lam, #{"p" => App1}}],
  X = cf_sem:eval( App2, ?THETA0 ),
  ?assertMatch( [{app, 4, 1, Lam, _}], X ).

app_non_final_result_preserves_app_test() ->
  Sign = {sign, [{param, {name, "out", false}, false}], []},
  Select = [{select, 1, 1, {fut, "f", 1234, [{param, {name, "out", false}, false}]}}],
  Cnd = [{cnd, 1, Select, [{var, 2, "x"}], [{var, 3, "y"}]}],
  Body = {natbody, #{"out" => Cnd}},
  Lam = {lam, 3, "g", Sign, Body},
  App = [{app, 4, 1, Lam, #{}}],
  X = cf_sem:eval( App, ?THETA0 ),
  ?assertMatch( App, X ).

app_tail_recursion_is_optimized_test() ->
  Sign = {sign, [{param, {name, "out", false}, false}], []},
  Select = [{select, 1, 1, {fut, "f", 1234, [{param, {name, "out", false}, false}]}}],
  Body = {natbody, #{"out" => Select}},
  Lam = {lam, 3, "g", Sign, Body},
  App = [{app, 4, 1, Lam, #{}}],
  X = cf_sem:eval( App, ?THETA0 ),
  ?assertMatch( Select, X ).

app_non_final_result_preserves_app_with_new_lam_test() ->
  CSign = {sign, [{param, {name, "out", false}, true}], []},
  CLam = {lam, 1, "f", CSign, {forbody, bash, "shalala"}},
  CApp = [{app, 2, 1, CLam, #{}}],
  Sign = {sign, [{param, {name, "out", false}, false}], []},
  Body = {natbody, #{"out" => [{cnd, 3, CApp, [{var, 4, "x"}], [{var, 5, "x"}]}],
                     "x" => [{str, "A"}]}},
  Lam = {lam, 7, "f", Sign, Body},
  App = [{app, 8, 1, Lam, #{}}],
  X = cf_sem:eval( App, ?THETA0 ),
  [{app, 8, 1, {lam, 7, "f", Sign, {natbody, BodyMap1}}, #{}}] = X,
  Val = maps:get( "out", BodyMap1 ),
  ?assertMatch( [{cnd, 3, [{select, 2, 1, _}], [{var, 4, "x"}], [{var, 5, "x"}]}], Val ).

nested_app_undergoes_reduction_test() ->
  Sign = {sign, [{param, {name, "out", false}, false}], []},
  Lam1 = {lam, 1, "f", Sign, {forbody, bash, "shalala"}},
  App1 = [{app, 2, 1, Lam1, #{}}],
  Body2 = {natbody, #{"out" => App1}},
  Lam2 = {lam, 3, "g", Sign, Body2},
  App2 = [{app, 4, 1, Lam2, #{}}],
  X = cf_sem:eval( App2, ?THETA0 ),
  [{select, 2, 1, {fut, "f", R, _}}] = X,
  Omega = #{{"out", R} => [{str, "A"}]},
  Y = cf_sem:eval( X, {#{}, fun mu/1, #{}, Omega} ),
  ?assertEqual( [{str, "A"}], Y ).

app_select_param_is_enumerated_test() ->
  Sign1 = {sign, [{param, {name, "out", false}, false}], []},
  Lam1 = {lam, 1, "f", Sign1, {forbody, bash, "shalala"}},
  Sign2 = {sign, [{param, {name, "out", false}, false}],
                 [{param, {name, "inp", false}, false}]},
  Body2 = {natbody, #{"out" => [{var, 2, "inp"}]}},
  Lam2 = {lam, 3, "g", Sign2, Body2},
  A0 = {app, 4, 1, Lam1, #{}},
  A = [A0, A0],
  B = [{app, 5, 1, Lam2, #{"inp" => A}}],
  X = cf_sem:eval( B, ?THETA0 ), 
  ?assertMatch( [{app, 5, 1, _, _}, {app, 5, 1, _, _}], X ).

app_str_is_evaluated_while_select_remains_test() ->
  Sign2 = {sign, [{param, {name, "out", false}, false}],
                 [{param, {name, "inp", false}, false}]},
  Body2 = {forbody, bash, "lalala"},
  Lam2 = {lam, 2, "g", Sign2, Body2},
  Fut = {fut, "h", 1234, [{param, {name, "y", false}, false}]},
  Select = {select, 4, 1, Fut},
  Str = {str, "blub"},
  A = [Str, Select],
  B = [{app, 3, 1, Lam2, #{"inp" => A}}],
  X = cf_sem:eval( B, ?THETA0 ), 
  ?assertMatch( [{select, 3, 1, {fut, "g", _, [{param, {name, "out", false}, false}]}},
                 {app, 3, 1, Lam2, #{"inp" := [Select]}}], X ).

% deftask find_clusters( cls( File ) : state( File ) ) {
%   cls = state;
% }
% mu0 = 1;
% cls = find_clusters( state: mu0 );
% cls;
identity_fn_should_resolve_var_test() ->
  Lo = [{param, {name, "cls", false}, false}],
  Li = [{param, {name, "state", false}, false}],
  Sign = {sign, Lo, Li},
  Body = {natbody, #{"cls" => [{var, 1, "state"}]}},
  Lam = {lam, 2, "find_clusters", Sign, Body},
  Fa = #{"state" => [{var, 3, "mu0"}]},
  App = {app, 4, 1, Lam, Fa},
  Rho = #{"cls" => [App], "mu0" => [{str, "1"}]},
  X = [{var, 6, "cls"}],
  ?assertEqual( [{str, "1"}], cf_sem:eval( X, {Rho, fun sem:mu/1, #{}, #{}} ) ).