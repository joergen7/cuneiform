%% -*- erlang -*-
%%
%% Cuneiform: A Functional Language for Large Scale Scientific Data Analysis
%%
%% Copyright 2016 Jörgen Brandt, Marc Bux, and Ulf Leser
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% @author Jörgen Brandt <brandjoe@hu-berlin.de>


-module( cf_sem ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).
-vsn( "2.2.1-snapshot" ).


-export( [eval/2, pnormal/1] ).

-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).
-endif.

%% =============================================================================
%% Abstract Syntax
%% =============================================================================

%% Expression %% ===============================================================

-type expr()    :: str() | var() | select() | cnd() | app().                    % (1)
-type str()     :: {str, S::string()}.                                          % (2)
-type var()     :: {var, Line::pos_integer(), N::string()}.                     % (3)
-type select()  :: {select, AppLine::pos_integer(), C::pos_integer(), U::fut()}.% (4)
-type fut()     :: {fut, LamName::string(), R::pos_integer(), Lo::[param()]}.   % (5)
-type cnd()     :: {cnd, Line::pos_integer(),                                   % (6)
                         Xc::[expr()], Xt::[expr()], Xe::[expr()]}.
-type app()     :: {app, AppLine::pos_integer(), C::pos_integer(),              % (7)
                         Lambda::lam() | var(), Fa::#{string() => [expr()]}}.

%% Lambda %% ===================================================================

-type lam()     :: {lam, Line::pos_integer(), Name::string(),                   % (8)
                         S::sign(), B::body()}.

% Task Signature
-type sign()    :: {sign, Lo::[param()], Li::[inparam()]}.                      % (9)
-type param()   :: {param, M::name(), Pl::boolean()}.                           % (10)
-type name()    :: {name, N::string(), Pf::boolean()}.
-type inparam() :: param() | correl().                                          % (11)
-type correl()  :: {correl, Lc::[name()]}.                                      % (12)

% Body
-type body()    :: natbody() | forbody().                                       % (13)
-type natbody() :: {natbody, Fb::#{string() => [expr()]}}.                      % (14)
-type forbody() :: {forbody, L::lang(), S::string()}.                           % (15)
-type lang()    :: bash | python | r.                                           % (16)

%% Argument Pair %% ============================================================

-type argpair() :: {L0::[inparam()], F::#{string() => [expr()]}}.               % (17)

%% Evaluation Context %% =======================================================

-type ctx()     :: {Rho   :: #{string() => [expr()]},                           % (18)
                    Mu    :: fun( ( app() ) -> fut() ),
                    Gamma :: #{string() => lam()},
                    Omega :: #{{string(), pos_integer()} => [expr()]}}.

%% =============================================================================
%% Predicates
%% =============================================================================

%% Finality %% =================================================================

-spec pnormal( X ) -> boolean()                                                  % (19)
when X :: #{string() => [expr()]} | [expr()] | expr().

pnormal( F ) when is_map( F )  -> pnormal( maps:values( F ) );                    % (20)
pnormal( L ) when is_list( L ) -> lists:all( fun pnormal/1, L );                  % (21,22)
pnormal( {str, _S} )           -> true;                                          % (23)
pnormal( _T )                  -> false.

%% Singularity %% ==============================================================

-spec psing( A::app() ) -> boolean().                                           % (24)

psing( {app, Line, _C, {lam, _, LamName, {sign, _Lo, Li}, _B}, Fa} ) ->         % (25)
  case psing_argpair( {Li, Fa} ) of
    {ok, P}    -> P;
    {error, N} ->
      throw( {Line, cf_sem,
              "argument "++N++" is unbound in application of "++LamName} )
  end.

-spec psing_argpair( Z::argpair() ) -> {ok, boolean()} | {error, string()}.                               % (26)

psing_argpair( {[], _F} ) -> {ok, true};                                        % (27)
psing_argpair( {[{param, _, Pl}|T], F} )                                        % (28)
when Pl ->
  psing_argpair( {T, F} );
psing_argpair( {[{param, {name, N, _}, _Pl}|T], F} ) ->                         % (29)
  case maps:is_key( N, F ) of
    false -> {error, N};
    true  ->
      case length( maps:get( N, F ) ) of
        1 -> psing_argpair( {T, F} );
        _ -> {ok, false}
      end
  end;
psing_argpair( _Z ) -> {ok, false}.

%% Enumerability %% ============================================================

-spec pen( X::expr()|[expr()] ) -> boolean().                                   % (30)

pen( X )when is_list( X ) -> lists:all( fun pen/1, X );                         % (31,32)
pen( {str, _S} )          -> true;                                              % (33)
pen( {cnd, _, _Xc, Xt, Xe} ) when length( Xt ) =:= 1, length( Xe ) =:= 1 ->     % (34)
  pen( Xt ) andalso pen( Xe );
pen( X={app, _, C, {lam, _, _, {sign, Lo, _Li}, _B}, _Fb} ) ->                  % (35)
  case psing( X ) of
    false -> false;
    true ->
      {param, _, Pl} = lists:nth( C, Lo ),
      not Pl
  end;
pen( {select, _, C, {fut, _, _R, Lo}} ) ->                                      % (36)
  {param, _, Pl} = lists:nth( C, Lo ),
  not Pl;
pen( _T ) -> false.

-spec pindep( X ) -> boolean()                                                  % (37)
when X :: #{string() => [expr()]} | [expr()] | expr().

pindep( Fa ) when is_map( Fa ) -> pindep( maps:values( Fa ) );                  % (38)
pindep( X ) when is_list( X )  -> lists:all( fun pindep/1, X );                 % (39,40)
pindep( {str, _} )             -> true;                                         % (41)
pindep( {select, _, _, _} )       -> true;                                      % (42)
pindep( {cnd, _, Xc, Xt, Xe} )    ->                                            % (43)
  pindep( Xc ) andalso pindep( Xt ) andalso pindep( Xe );
pindep( {app, _, _, _, Fa} )      -> pindep( Fa );                              % (44)
pindep( _ )                    -> false.

%% =============================================================================
%% Evaluation
%% =============================================================================

%% The eval Function %% ========================================================

-spec eval( X::[expr()], Theta::ctx() ) -> [expr()].                            % (37)

eval( X, Theta ) ->
  X1 = step( X, Theta ),
  case X1 of
    X -> X;                                                                     % (38)
    _ -> eval( X1, Theta )                                                      % (39)
  end.

%% Reduction Rules %% ==========================================================

-spec step_assoc( F, Theta ) -> #{string() => [expr()]}                         % (40)
when F     :: #{string() => [expr()]},
     Theta :: ctx().

step_assoc( F, Theta ) when is_map( F ) ->                                      % (41)
  maps:map( fun( _N, X ) -> step( X, Theta ) end, F ).


-spec step( X, Theta ) -> [expr()]                                              % (42)
when X     :: #{string() => [expr()]} | [expr()] | expr(),
     Theta :: ctx().


% Expression List
step( X, Theta ) when is_list( X ) ->                                           % (43,44)
  lists:flatmap( fun( Y ) -> step( Y, Theta ) end, X );

% String Literal
step( X={str, _S}, _Theta ) -> [X];                                             % (45)

% Variable
step( {var, _, N}, {Rho, _Mu, _Gamma, _Omega} ) ->                              % (46)
  case maps:is_key( N, Rho ) of
    true  -> maps:get( N, Rho );
    false -> throw( {1, cf_sem, "unbound variable "++N} )
  end;

% Future Channel Selection
step( S={select, _, C, {fut, _, R, Lo}}, {_Rho, _Mu, _Gamma, Omega} ) ->        % (47,48)
  {param, {name, N, _}, _} = lists:nth( C, Lo ),
  maps:get( {N, R}, Omega, [S] );

% Conditional
step( {cnd, _, [], _Xt, Xe}, _Theta ) -> Xe;                                    % (49)
step( {cnd, Line, Xc=[_|_], Xt, Xe}, Theta ) ->
  case pnormal( Xc ) of
    false -> [{cnd, Line, step( Xc, Theta ), Xt, Xe}];                          % (50)
    true  -> Xt                                                                 % (51)
  end;

% Application
step( {app, Line, C, {var, _, N}, Fa}, {_Rho, _Mu, Gamma, _Omega} ) ->          % (52)
  case maps:is_key( N, Gamma ) of
    true  -> [{app, Line, C, maps:get( N, Gamma ), Fa}];
    false -> throw( {Line, cf_sem, "undefined task "++N} )
  end;

step( X={app, AppLine, C,
      Lambda={lam, LamLine, LamName, S={sign, Lo, _Li}, B}, Fa},
      Theta={_Rho, Mu, Gamma, Omega} ) ->
  case psing( X ) of
    false -> enum_app( {app, AppLine, C, Lambda, step_assoc( Fa, Theta )} );    % (53)
    true  ->
      case pnormal( Fa ) of
        false -> [{app, AppLine, C, Lambda, step_assoc( Fa, Theta )}];          % (54)
        true  ->
          case B of
            {forbody, _L, _Z} -> [{select, AppLine, C, apply( Mu, [X] )}];      % (55)
            {natbody, Fb} ->
              {param, {name, N, _}, Pl} = lists:nth( C, Lo ),
              case maps:is_key( N, Fb ) of
                false -> throw( {LamLine, cf_sem, "undefined output parameter "++N++" in task "++LamName} );
                true  ->
                  V0 = maps:get( N, Fb ),
                  V1 = step( V0, {maps:merge( Fb, Fa ), Mu, Gamma, Omega} ),
                  case pindep( V1 ) of
                    false -> [{app, AppLine, C,                                     % (56)
                                    {lam, LamLine, LamName, S,
                                          {natbody, Fb#{ N => V1}}},
                                    Fa}];
                    true  ->
                      case Pl orelse length( V1 ) =:= 1 of
                        true  -> V1;                                                % (57)
                        false -> error( output_sign_mismatch )
                      end
                  end
              end
          end
      end
  end.



%% =============================================================================
%% Enumeration
%% =============================================================================

%% The enum Function %%

-spec enum_app( A::app() ) -> [app()].                                          % (58)

enum_app( {app, AppLine, C, {lam, LamLine, LamName, {sign, Lo, Li}, B}, Fa} ) ->% (59)
  [{app, AppLine, C,
         {lam, LamLine, LamName,
               {sign, Lo, L1}, B}, F1} || {L1, F1} <- enum( [{Li, Fa}] )].

-spec enum( Z::[argpair()] ) -> [argpair()].                                    % (60)

enum( Z ) ->
  Z1 = estep( Z ),
  case Z1 of
    Z -> Z;                                                                     % (61)
    _ -> enum( Z1 )                                                             % (62)
  end.

%% Enumeration Rules %%

-spec estep( Z::[argpair()] ) -> [argpair()].                                   % (63)

estep( Z ) ->                                                                   % (64,65)
  lists:flatmap( fun( {Li, F} ) -> estep_param_lst( Li, F ) end, Z ).

-spec estep_param_lst( Li::[inparam()], F::#{string() => [expr()]} ) -> [argpair()].      % (66)

estep_param_lst( [], F ) -> [{[], F}];                                                    % (67)
estep_param_lst( [H={param, _, Pl}|T], F ) when Pl -> aug( estep_param_lst( T, F ), H );           % (68)
estep_param_lst( L=[H={param, {name, N, _}, _Pl}|T], F ) ->
  V = maps:get( N, F ),
  case pen( V ) of
    false -> [{L, F}];                                                          % (69)
    true  ->
      case length( V ) of
        1 -> aug( estep_param_lst( T, F ), H );                                           % (70)
        _ -> [{L, maps:put( N, [X], F )} || X <- V]                             % (71)
      end
  end;
estep_param_lst( L=[H={correl, Lc}|T], F ) when length( Lc ) > 1 ->
  Pen = pen( [maps:get( N, F ) || {name, N, _} <- Lc] ),
  case Pen of
    false -> [{L, F}];                                                          % (72)
    true  ->
      Z = corr( Lc, F ),
      aug( [{T, G} || G <- Z], H )                                              % (73)
  end.



%% Augmentation %%

-spec aug( Z::[argpair()], A::inparam() ) -> [argpair()].                       % (74)

aug( Z, A ) -> [aug_argpair( Y, A ) || Y <- Z].                                 % (75)

-spec aug_argpair( Y::argpair(), A::inparam() ) -> argpair().                   % (76)

aug_argpair( {L0, F}, A={param, _, _Pl} ) -> {[A|L0], F};                       % (77)
aug_argpair( {L0, F}, {correl, Lc} ) ->                                         % (78)
  L1 = [{param, M, false} || M <- Lc],
  {L1++L0, F}.

%% Correlation %%

-spec corr( Lc, F ) -> [#{string() => [expr()]}]
when Lc :: [name()],
     F  :: #{string() => [expr()]}.

corr( Lc, F ) ->
  case corrstep( Lc, F, F ) of
    []    -> [];
    [H,T] -> [H|corr( Lc, T )]
  end.

-spec corrstep( Lc, Facc, F0 ) -> [#{string() => [expr()]}]                     % (79)
when Lc   :: [name()],
     Facc :: #{string() => [expr()]},
     F0   :: #{string() => [expr()]}.

corrstep( [], Facc, F0 ) -> [Facc, F0];                                         % (80)
corrstep( [{name, H, _}|T], Facc, F0 ) ->
  case maps:get( H, F0 ) of
    []    -> [];                                                                % (81)
    [A|B] -> corrstep( T, Facc#{H => [A]}, F0#{H => B})                         % (82)
  end.




%% =============================================================================
%% Unit Tests
%% =============================================================================

-ifdef( TEST ).


%% =============================================================================
%% Predicates
%% =============================================================================

%% Finality %%

str_should_be_normal_test() ->
  S = {str, "blub"},
  ?assert( pnormal( S ) ).

app_should_not_be_normal_test() ->
  A = {app, 12, 1, {var, "f"}, #{}},
  ?assertNot( pnormal( A ) ).

cnd_should_not_be_normal_test() ->
  C = {cnd, 12, [{str, "a"}], [{str, "b"}], [{str, "c"}]},
  ?assertNot( pnormal( C ) ).

select_should_not_be_normal_test() ->
  Fut = {fut, "f", 1234, [{param, "out", false}]},
  S = {select, 12, 1, Fut},
  ?assertNot( pnormal( S ) ).

var_should_not_be_normal_test() ->
  V = {var, 12, "x"},
  ?assertNot( pnormal( V ) ).

all_str_should_be_normal_test() ->
  X = [{str, "bla"}, {str, "blub"}],
  ?assert( pnormal( X ) ).

empty_lst_should_be_normal_test() ->
  ?assert( pnormal( [] ) ).

one_var_lst_should_not_be_normal_test() ->
  X = [{str, "bla"}, {str, "blub"}, {var, 12, "x"}],
  ?assertNot( pnormal( X ) ).

all_var_lst_should_not_be_normal_test() ->
  X = [{var, 10, "bla"}, {var, 11, "blub"}, {var, 12, "x"}],
  ?assertNot( pnormal( X ) ).

empty_map_should_be_normal_test() ->
  ?assert( pnormal( #{} ) ).

only_str_map_should_be_normal_test() ->
  M = #{"x" => [{str, "bla"}, {str, "blub"}], "y" => [{str, "shalala"}]},
  ?assert( pnormal( M ) ).

one_var_map_should_not_be_normal_test() ->
  M = #{"x" => [{str, "bla"}, {str, "blub"}],
        "y" => [{str, "shalala"}, {var, 12, "x"}]},
  ?assertNot( pnormal( M ) ).

all_var_map_should_not_be_normal_test() ->
  M = #{"x" => [{var, 10, "bla"}, {var, 11, "blub"}],
        "y" => [{var, 12, "shalala"}, {var, 13, "x"}]},
  ?assertNot( pnormal( M ) ).

%% Singularity %%

app_without_arg_should_be_singular_test() ->
  S = {sign, [{param, {name, "out", false}, false}], []},
  B = {forbody, bash, "shalala"},
  Lam = {lam, 12, "f", S, B},
  App = {app, 13, 1, Lam, #{}},
  ?assert( psing( App ) ).

app_binding_single_str_should_be_singular_test() ->
  S = {sign, [{param, {name, "out", false}, false}], [{param, {name, "x", false}, false}]},
  B = {forbody, bash, "shalala"},
  Lam = {lam, 12, "f", S, B},
  App = {app, 13, 1, Lam, #{"x" => [{str, "bla"}]}},
  ?assert( psing( App ) ).

app_binding_str_lst_should_not_be_singular_test() ->
  S = {sign, [{param, {name, "out", false}, false}], [{param, {name, "x", false}, false}]},
  B = {forbody, bash, "shalala"},
  Lam = {lam, 12, "f", S, B},
  App = {app, 13, 1, Lam, #{"x" => [{str, "bla"}, {str, "blub"}]}},
  ?assertNot( psing( App ) ).

app_with_only_aggregate_args_should_be_singular_test() ->
  S = {sign, [{param, {name, "out", false}, false}], [{param, {name, "x", false}, true}]},
  B = {forbody, bash, "shalala"},
  Lam = {lam, 12, "f", S, B},
  App = {app, 13, 1, Lam, #{"x" => [{str, "bla"}, {str, "blub"}]}},
  ?assert( psing( App ) ).

%% Enumerability %%

empty_lst_should_be_enumerable_test() ->
  ?assert( pen( [] ) ).

str_lst_should_be_enumerable_test() ->
  ?assert( pen( [{str, "a"}, {str, "b"}] ) ).

one_var_lst_should_not_be_enumerable_test() ->
  ?assertNot( pen( [{str, "a"}, {var, 12, "x"}] ) ).

all_var_lst_should_not_be_enumerable_test() ->
  ?assertNot( pen( [{var, 12, "x"}, {var, 13, "y"}] ) ).

str_should_be_enumerable_test() ->
  ?assert( pen( {str, "blub"} ) ).

single_str_branch_cnd_should_be_enumerable_test() ->
  ?assert( pen( {cnd, 12, [], [{str, "a"}], [{str, "b"}]} ) ).

empty_then_branch_cnd_should_not_be_enumerable_test() ->
  ?assertNot( pen( {cnd, 12, [], [], [{str, "b"}]} ) ).

empty_else_branch_cnd_should_not_be_enumerable_test() ->
  ?assertNot( pen( {cnd, 12, [], [{str, "a"}]} ) ).

select_non_lst_app_should_be_enumerable_test() ->
  Sign = {sign, [{param, {name, "out", false}, false}], []},
  Body = {forbody, bash, "shalala"},
  Lam = {lam, 12, "f", Sign, Body},
  App = {app, 13, 1, Lam, #{}},
  ?assert( pen( App ) ).

select_lst_app_should_not_be_enumerable_test() ->
  Sign = {sign, [{param, {name, "out", false}, true}], []},
  Body = {forbody, bash, "shalala"},
  Lam = {lam, 12, "f", Sign, Body},
  App = {app, 13, 1, Lam, #{}},
  ?assertNot( pen( App ) ).

non_singular_app_should_not_be_enumerable_test() ->
  Sign = {sign, [{param, {name, "out", false}, false}], [{param, {name, "x", false}, false}]},
  Body = {forbody, bash, "shalala"},
  Lam = {lam, 12, "f", Sign, Body},
  Fa = #{"x" => [{str, "bla"}, {str, "blub"}]},
  App = {app, 13, 1, Lam, Fa},
  ?assertNot( pen( App ) ).

non_lst_select_should_be_enumerable_test() ->
  Lo = [{param, {name, "out", false}, false}],
  Fut = {fut, "f", 1234, Lo},
  Select = {select, 12, 1, Fut},
  ?assert( pen( Select ) ).

lst_select_should_not_be_enumerable_test() ->
  Lo = [{param, {name, "out", false}, true}],
  Fut = {fut, "f", 1234, Lo},
  Select = {select, 12, 1, Fut},
  ?assertNot( pen( Select ) ).

%% =============================================================================
%% Enumeration
%% =============================================================================

%% The enum Function %%

enum_app_without_app_does_nothing_test() ->
  S = {sign, [{param, {param, "out", false}, false}], []},
  B = {forbody, bash, "shalala"},
  Lam = {lam, 1, "f", S, B},
  App = {app, 2, 1, Lam, #{}},
  ?assertEqual( [App], enum_app( App ) ).

%% Enumeration Rules %%

%% Augmentation %%

can_aug_with_param_test() ->
  L0 = [{param, {name, "b", false}, false}, {param, {name, "c", false}, false}],
  F = #{"a" => [{str, "1"}], "b" => [{str, "2"}], "c" => [{str, "3"}]},
  Pair = {L0, F},
  I = {param, {name, "a", false}, false},
  L1 = [I|L0],
  ?assertEqual( {L1, F}, aug_argpair( Pair, I ) ).

can_aug_with_correl_test() ->
  L0 = [{param, {name, "b", false}, false}, {param, {name, "c", false}, false}],
  F = #{"a1" => [{str, "11"}],
        "a2" => [{str, "12"}],
        "b" => [{str, "2"}],
        "c" => [{str, "3"}]},
  Pair = {L0, F},
  I = {correl, [{name, "a1", false}, {name, "a2", false}]},
  L1 = [{param, {name, "a1", false}, false}, {param, {name, "a2", false}, false}|L0],
  ?assertEqual( {L1, F}, aug_argpair( Pair, I ) ).

can_augment_empty_inparamlst_with_param_test() ->
  F1 = #{"a" => [{str, "x1"}]},
  F2 = #{"a" => [{str, "y1"}]},
  PairList = [{[], F1}, {[], F2}],
  I = {param, {name, "a", false}, false},
  L1 = [{param, {name, "a", false}, false}],
  ?assertEqual( [{L1, F1}, {L1, F2}], aug( PairList, I ) ).

can_augment_inparamlst_with_param_test() ->
  L0 = [{param, {name, "b", false}, false}, {param, {name, "c", false}, false}],
  F1 = #{"a" => [{str, "x1"}], "b" => [{str, "x2"}], "c" => [{str, "x3"}]},
  F2 = #{"a" => [{str, "y1"}], "b" => [{str, "y2"}], "c" => [{str, "y3"}]},
  PairList = [{L0, F1}, {L0, F2}],
  I = {param, {name, "a", false}, false},
  L1 = [{param, {name, "a", false}, false}|L0],
  ?assertEqual( [{L1, F1}, {L1, F2}], aug( PairList, I ) ).

can_augment_inparamlst_with_correl_test() ->
  L0 = [{param, {name, "b", false}, false}, {param, {name, "c", false}, false}],
  F1 = #{"a1" => [{str, "x11"}],
         "a2" => [{str, "x12"}],
         "b" => [{str, "x2"}],
         "c" => [{str, "x3"}]},
  F2 = #{"a1" => [{str, "y11"}],
         "a2" => [{str, "y12"}],
         "b" => [{str, "y2"}],
         "c" => [{str, "y3"}]},
  PairList = [{L0, F1}, {L0, F2}],
  I = {correl, [{name, "a1", false}, {name, "a2", false}]},
  L1 = [{param, {name, "a1", false}, false}, {param, {name, "a2", false}, false}|L0],
  ?assertEqual( [{L1, F1}, {L1, F2}], aug( PairList, I ) ).

%% Correlation %%

corrstep_should_separate_first_value_single_test() ->
  Lc = [{name, "a", false}],
  F0 = #{"a" => [{str, "1"}, {str, "2"}, {str, "3"}]},
  Y = [#{"a" => [{str, "1"}]}, #{"a" => [{str, "2"}, {str, "3"}]}],
  X = corrstep( Lc, F0, F0 ),
  ?assertEqual( Y, X ).

corrstep_should_separate_first_value_two_test() ->
  Lc = [{name, "a", false}, {name, "b", false}],
  F0 = #{"a" => [{str, "1"}, {str, "2"}, {str, "3"}], "b" => [{str, "A"}, {str, "B"}, {str, "C"}]},
  Y = [#{"a" => [{str, "1"}], "b" => [{str, "A"}]}, #{"a" => [{str, "2"}, {str, "3"}], "b" => [{str, "B"}, {str, "C"}]}],
  X = corrstep( Lc, F0, F0 ),
  ?assertEqual( Y, X ).



-endif.