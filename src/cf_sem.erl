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
-type param()   :: {param, M::name(), Pl::boolean()}.                           % (4)
-type fut()     :: {fut, LamName::string(), R::pos_integer(), Lo::[param()]}.   % (5)
-type select()  :: {select, AppLine::pos_integer(), C::pos_integer(), U::fut()}.% (6)
-type cnd()     :: {cnd, Line::pos_integer(),                                   % (7)
                         Xc::[expr()], Xt::[expr()], Xe::[expr()]}.
-type app()     :: {app, AppLine::pos_integer(), C::pos_integer(),              % (8)
                         Lambda::lam() | var(), Fa::#{string() => [expr()]}}.

%% Task Signature %% ===========================================================

-type correl()  :: {correl, Lc::[name()]}.                                      % (9)
-type inparam() :: param() | correl().                                          % (10)
-type sign()    :: {sign, Lo::[param()], Li::[inparam()]}.                      % (11)

%% Lambda Term %% ==============================================================

-type lam()     :: {lam, LamLine::pos_integer(), LamName::string(),             % (12)
                         S::sign(), B::body()}.


-type name()    :: {name, N::string(), Pf::boolean()}.

% Task Body
-type body()    :: natbody() | forbody().                                       % (13)
-type natbody() :: {natbody, Fb::#{string() => [expr()]}}.                      % (14)
-type forbody() :: {forbody, L::lang(), S::string()}.                           % (15)
-type lang()    :: bash | python | r.                                           % (16)

%% Evaluation Context %% =======================================================

-type ctx()     :: {Rho   :: #{string() => [expr()]},                           % (17)
                    Mu    :: fun( ( app() ) -> fut() ),
                    Gamma :: #{string() => lam()},
                    Omega :: #{{string(), pos_integer()} => [expr()]}}.

%% =============================================================================
%% Predicates
%% =============================================================================

%% Normal Form %% ==============================================================

-spec pnormal( X ) -> boolean()                                                 % (18)
when X :: #{string() => [expr()]} | [expr()] | expr().

pnormal( F ) when is_map( F )  -> pnormal( maps:values( F ) );                  % (19)
pnormal( L ) when is_list( L ) -> lists:all( fun pnormal/1, L );                % (20,21)
pnormal( {str, _S} )           -> true;                                         % (22)
pnormal( _T )                  -> false.

%% Singularity %% ==============================================================

-spec psing( A::app() ) -> boolean().                                           % (61)

psing( {app, _, _, {lam, _, _, {sign, _, []}, _}, _} ) -> true;                 % (62)
psing( {app, AppLine, C,                                                        % (63)
             {lam, LamLine, LamName, {sign, Lo, [{param, _, Pl}|T]}, B},
             Fa} )
when Pl ->
  psing( {app, AppLine, C, {lam, LamLine, LamName, {sign, Lo, T}, B}, Fa} );
psing( {app, AppLine, C,                                                        % (64)
             {lam, LamLine, LamName, {sign, Lo, [{param, {name, N, _}, _}|T]}, B},
             Fa} ) ->
  case maps:is_key( N, Fa ) of
    false -> throw( {AppLine, cf_sem,
               "argument "++N++" is unbound in application of "++LamName} );
    true  ->
      case length( maps:get( N, Fa ) ) of
        1 -> psing( {app, AppLine, C, {lam, LamLine, LamName, {sign, Lo, T}, B}, Fa} );
        _ -> false
      end
  end;
psing( _ ) -> false.

%% Enumerability %% ============================================================

-spec pen( X::expr()|[expr()] ) -> boolean().                                   % (65)

pen( X )when is_list( X ) -> lists:all( fun pen/1, X );                         % (66,67)
pen( {str, _S} )          -> true;                                              % (68)
pen( {cnd, _, _Xc, Xt, Xe} ) when length( Xt ) =:= 1, length( Xe ) =:= 1 ->     % (69)
  pen( Xt ) andalso pen( Xe );
pen( X={app, _, C, {lam, _, _, {sign, Lo, _Li}, _B}, _Fb} ) ->                  % (70)
  case psing( X ) of
    false -> false;
    true ->
      {param, _, Pl} = lists:nth( C, Lo ),
      not Pl
  end;
pen( {select, _, C, {fut, _, _R, Lo}} ) ->                                      % (71)
  {param, _, Pl} = lists:nth( C, Lo ),
  not Pl;
pen( _T ) -> false.

%% Context Independence %% =====================================================

-spec pindep( X ) -> boolean()                                                  % (81)
when X :: #{string() => [expr()]} | [expr()] | expr().

pindep( Fa ) when is_map( Fa ) -> pindep( maps:values( Fa ) );                  % (82)
pindep( X ) when is_list( X )  -> lists:all( fun pindep/1, X );                 % (83,84)
pindep( {str, _} )             -> true;                                         % (85)
pindep( {select, _, _, _} )    -> true;                                         % (86)
pindep( {cnd, _, Xc, Xt, Xe} ) ->                                               % (87)
  pindep( Xc ) andalso pindep( Xt ) andalso pindep( Xe );
pindep( {app, _, _, _, Fa} )   -> pindep( Fa );                                 % (88)
pindep( _ )                    -> false.

%% =============================================================================
%% Evaluation
%% =============================================================================

%% The eval Function %% ========================================================

-spec eval( X::[expr()], Theta::ctx() ) -> [expr()].

eval( X, Theta ) ->
  X1 = step( X, Theta ),
  case X1 of
    X -> X;
    _ -> eval( X1, Theta )
  end.

%% Reduction Rules %% ==========================================================



-spec step( X, Theta ) -> #{string() => [expr()]} | [expr()]                                              % (42)
when X     :: #{string() => [expr()]} | [expr()] | expr(),
     Theta :: ctx().

% Argument map
step( Fa, Theta ) when is_map( Fa ) ->                                          % (23,24)
  maps:map( fun( _N, X ) -> step( X, Theta ) end, Fa );

% Expression List
step( X, Theta ) when is_list( X ) ->                                           % (25,26)
  lists:flatmap( fun( Y ) -> step( Y, Theta ) end, X );

% String Literal
step( X={str, _S}, _Theta ) -> [X];                                             % (27)

% Variable
step( {var, _, N}, {Rho, _Mu, _Gamma, _Omega} ) ->                              % (28)
  case maps:is_key( N, Rho ) of
    true  -> maps:get( N, Rho );
    false -> throw( {1, cf_sem, "unbound variable "++N} )
  end;

% Future Channel Selection
step( S={select, _, C, {fut, _, R, Lo}}, {_Rho, _Mu, _Gamma, Omega} ) ->        % (29,30)
  {param, {name, N, _}, _} = lists:nth( C, Lo ),
  maps:get( {N, R}, Omega, [S] );

% Conditional
step( {cnd, _, [], _Xt, Xe}, _Theta ) -> Xe;                                    % (31)
step( {cnd, Line, Xc=[_|_], Xt, Xe}, Theta ) ->
  case pnormal( Xc ) of
    false -> [{cnd, Line, step( Xc, Theta ), Xt, Xe}];                          % (32)
    true  -> Xt                                                                 % (33)
  end;

% Application (early enumeration and tail recursion)
step( {app, Line, C, {var, _, N}, Fa}, {_Rho, _Mu, Gamma, _Omega} ) ->          % (52)
  case maps:is_key( N, Gamma ) of
    true  -> [{app, Line, C, maps:get( N, Gamma ), Fa}];
    false -> throw( {Line, cf_sem, "undefined task "++N} )
  end;

step( X={app, AppLine, C,
              Lambda={lam, LamLine, LamName, S={sign, Lo, _Li}, B},
              Fa},
      Theta={_Rho, Mu, Gamma, Omega} ) ->
  case psing( X ) of
    false -> enum( [{app, AppLine, C, Lambda, step( Fa, Theta )}] );            % (89)
    true  ->
      case B of
        {forbody, _L, _Z} ->
          case pnormal( Fa ) of
            false -> [{app, AppLine, C, Lambda, step( Fa, Theta )}];            % (90)
            true  -> [{select, AppLine, C, apply( Mu, [X] )}]                   % (91)
          end;
        {natbody, Fb} ->
          case pindep( Fa ) of
            false -> [{app, AppLine, C, Lambda, step( Fa, Theta )}];            % (92)
            true  ->
              {param, {name, N, _}, Pl} = lists:nth( C, Lo ),
              case maps:is_key( N, Fb ) of
                false -> throw( {LamLine, cf_sem,
                           "undefined output parameter "++N++" in task "++LamName} );
                true  ->
                  #{N := V0} = Fb,
                  V1 = step( V0, {maps:merge( Fb, Fa ), Mu, Gamma, Omega} ),
                  case pindep( V1 ) of
                    false -> [{app, AppLine, C,                                 % (93)
                                    {lam, LamLine, LamName, S,
                                          {natbody, Fb#{ N => V1 }}},
                                    Fa}];
                    true  ->
                      case Pl orelse length( V1 ) =:= 1 of
                        true  -> V1;                                            % (94)
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

-spec enum( A::[app()] ) -> [app()].                                            % (40)

enum( Z ) ->
  Z1 = estep( Z ),
  case Z1 of
    Z -> Z;                                                                     % (41)
    _ -> enum( Z1 )                                                             % (42)
  end.

%% Enumeration Rules (early enumeration) %%

-spec estep( A ) -> [app()]                                                     % (43)
when A :: app() | [app()].

estep( A ) when is_list( A ) ->                                                 % (44,45)
  lists:flatmap( fun( B ) -> estep( B ) end, A );
estep( X={app, _, _, {lam, _, _, {sign, _, []}, _}, _} ) -> [X];                % (46)
  
estep( {app, AppLine, C,                                                        % (47)
             {lam, LamLine, LamName, {sign, Lo, [H={param, _, Pl}|T]}, B}, Fa} )
when Pl ->
  aug_lst( estep( {app, AppLine, C, {lam, LamLine, LamName, {sign, Lo, T}, B}, Fa} ), H );
estep( X={app, AppLine, C,
               {lam, LamLine, LamName,
                     {sign, Lo, Li=[H={param, {name, N, _}, _Pl}|T]},
                     B},
               Fa} ) ->
  #{ N := V } = Fa,
  case pen( V ) of
    true  ->
      case length( V ) of
        1 -> aug_lst( estep( {app, AppLine, C,                                  % (72)
                                   {lam, LamLine, LamName,
                                         {sign, Lo, T},
                                         B},
                                   Fa} ), H );
        _ -> [{app, AppLine, C,                                                 % (73)
                    {lam, LamLine, LamName,
                          {sign, Lo, Li},
                          B},
                    Fa#{ N => [Y] }} || Y <- V]
      end;
    false -> [X]                                                                % (74)
  end;
estep( X={app, AppLine, C,
               {lam, LamLine, LamName,
                     {sign, Lo, [H={correl, Lc}|T]}, B}, Fa} )
when length( Lc ) > 1 ->
  Pen = pen( [maps:get( N, Fa ) || {name, N, _} <- Lc] ),
  case Pen of
    false -> [X];                                                               % (75)
    true  ->
      Z = corr( Lc, Fa ),
      aug_lst( [{app, AppLine, C,                                               % (76)
                      {lam, LamLine, LamName,
                            {sign, Lo, T},
                            B},
                      G} || G <- Z], H )
  end.



%% Augmentation %%

-spec aug_lst( Z::[app()], A::inparam() ) -> [app()].                           % (50)

aug_lst( Z, A ) -> [aug( X, A ) || X <- Z].                                     % (51)

-spec aug( X::app(), A::inparam() ) -> app().                                   % (52)

aug( {app, AppLine, C, {lam, LamLine, LamName, {sign, Lo, Li}, B}, Fa},         % (53)
     A={param, _, _Pl} ) ->
  {app, AppLine, C, {lam, LamLine, LamName, {sign, Lo, [A|Li]}, B}, Fa};
aug( {app, AppLine, C, {lam, LamLine, LamName, {sign, Lo, Li}, B}, Fa},         % (54)
     {correl, Lc} ) ->
  L1 = [{param, N, false} || N <- Lc],
  {app, AppLine, C, {lam, LamLine, LamName, {sign, Lo, L1++Li}, B}, Fa}.

%% Correlation %%

-spec corr( Lc, F ) -> [#{string() => [expr()]}]
when Lc :: [name()],
     F  :: #{string() => [expr()]}.

corr( Lc=[{name, N, _}|_], F ) ->
  case maps:get( N, F ) of
    [] -> [];
    _  ->
      {Fstar, Fminus} = corrstep( Lc, F, F ),
      [Fstar|corr( Lc, Fminus )]
  end.

-spec corrstep( Lc, Fstar, Fminus ) ->                                          % (58)
  {#{string() => [expr()]}, #{string() => [expr()]}}
when Lc     :: [name()],
     Fstar  :: #{string() => [expr()]},
     Fminus :: #{string() => [expr()]}.

corrstep( [], Fstar, Fminus ) -> {Fstar, Fminus};                               % (59)
corrstep( [{name, N, _}|T], Fstar, Fminus ) ->                                  % (60)
  #{ N := [A|B] } = Fminus,
  corrstep( T, Fstar#{ N => [A] }, Fminus#{ N => B } ).




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

enum_without_app_does_nothing_test() ->
  S = {sign, [{param, {param, "out", false}, false}], []},
  B = {forbody, bash, "shalala"},
  Lam = {lam, 1, "f", S, B},
  App = {app, 2, 1, Lam, #{}},
  ?assertEqual( [App], enum( App ) ).

%% Enumeration Rules %%

%% Augmentation %%

can_aug_with_param_test() ->
  Lo = [{param, {name, "out", false}, false}],
  B = {forbody, bash, "blub"},
  L0 = [{param, {name, "b", false}, false}, {param, {name, "c", false}, false}],
  F = #{"a" => [{str, "1"}], "b" => [{str, "2"}], "c" => [{str, "3"}]},
  App = {app, 10, 1, {lam, 20, "f", {sign, Lo, L0}, B}, F},
  I = {param, {name, "a", false}, false},
  L1 = [I|L0],
  ?assertEqual( {app, 10, 1, {lam, 20, "f", {sign, Lo, L1}, B}, F}, aug( App, I ) ).

can_aug_with_correl_test() ->
  Lo = [{param, {name, "out", false}, false}],
  B = {forbody, bash, "blub"},
  L0 = [{param, {name, "b", false}, false}, {param, {name, "c", false}, false}],
  F = #{"a1" => [{str, "11"}],
        "a2" => [{str, "12"}],
        "b" => [{str, "2"}],
        "c" => [{str, "3"}]},
  App = {app, 10, 1, {lam, 20, "f", {sign, Lo, L0}, B}, F},
  I = {correl, [{name, "a1", false}, {name, "a2", false}]},
  L1 = [{param, {name, "a1", false}, false}, {param, {name, "a2", false}, false}|L0],
  ?assertEqual( {app, 10, 1, {lam, 20, "f", {sign, Lo, L1}, B}, F}, aug( App, I ) ).

can_augment_empty_inparamlst_with_param_test() ->
  Lo = [{param, {name, "out", false}, false}],
  B = {forbody, bash, "blub"},
  F1 = #{"a" => [{str, "x1"}]},
  F2 = #{"a" => [{str, "y1"}]},
  AppList = [{app, 10, 1, {lam, 20, "f", {sign, Lo, []}, B}, F1},
             {app, 30, 1, {lam, 40, "g", {sign, Lo, []}, B}, F2}],
  I = {param, {name, "a", false}, false},
  L1 = [{param, {name, "a", false}, false}],
  ?assertEqual( [{app, 10, 1, {lam, 20, "f", {sign, Lo, L1}, B}, F1},
                 {app, 30, 1, {lam, 40, "g", {sign, Lo, L1}, B}, F2}],
                aug_lst( AppList, I ) ).

can_augment_inparamlst_with_param_test() ->
  Lo = [{param, {name, "out", false}, false}],
  B = {forbody, bash, "blub"},
  L0 = [{param, {name, "b", false}, false}, {param, {name, "c", false}, false}],
  F1 = #{"a" => [{str, "x1"}], "b" => [{str, "x2"}], "c" => [{str, "x3"}]},
  F2 = #{"a" => [{str, "y1"}], "b" => [{str, "y2"}], "c" => [{str, "y3"}]},
  AppList = [{app, 10, 1, {lam, 20, "f", {sign, Lo, L0}, B}, F1},
             {app, 30, 1, {lam, 40, "g", {sign, Lo, L0}, B}, F2}],
  I = {param, {name, "a", false}, false},
  L1 = [{param, {name, "a", false}, false}|L0],
  X = [{app, 10, 1, {lam, 20, "f", {sign, Lo, L1}, B}, F1},
       {app, 30, 1, {lam, 40, "g", {sign, Lo, L1}, B}, F2}],
  ?assertEqual( X, aug_lst( AppList, I ) ).

can_augment_inparamlst_with_correl_test() ->
  Lo = [{param, {name, "out", false}, false}],
  B = {forbody, bash, "blub"},
  L0 = [{param, {name, "b", false}, false}, {param, {name, "c", false}, false}],
  F1 = #{"a1" => [{str, "x11"}],
         "a2" => [{str, "x12"}],
         "b" => [{str, "x2"}],
         "c" => [{str, "x3"}]},
  F2 = #{"a1" => [{str, "y11"}],
         "a2" => [{str, "y12"}],
         "b" => [{str, "y2"}],
         "c" => [{str, "y3"}]},
  AppList = [{app, 10, 1, {lam, 20, "f", {sign, Lo, L0}, B}, F1},
             {app, 30, 1, {lam, 40, "g", {sign, Lo, L0}, B}, F2}],
  I = {correl, [{name, "a1", false}, {name, "a2", false}]},
  L1 = [{param, {name, "a1", false}, false}, {param, {name, "a2", false}, false}|L0],
  X = [{app, 10, 1, {lam, 20, "f", {sign, Lo, L1}, B}, F1},
       {app, 30, 1, {lam, 40, "g", {sign, Lo, L1}, B}, F2}],
  ?assertEqual( X, aug_lst( AppList, I ) ).

%% Correlation %%

corrstep_should_separate_first_value_single_test() ->
  Lc = [{name, "a", false}],
  F0 = #{"a" => [{str, "1"}, {str, "2"}, {str, "3"}]},
  Y = {#{"a" => [{str, "1"}]}, #{"a" => [{str, "2"}, {str, "3"}]}},
  X = corrstep( Lc, F0, F0 ),
  ?assertEqual( Y, X ).

corrstep_should_separate_first_value_two_test() ->
  Lc = [{name, "a", false}, {name, "b", false}],
  F0 = #{"a" => [{str, "1"}, {str, "2"}, {str, "3"}], "b" => [{str, "A"}, {str, "B"}, {str, "C"}]},
  Y = {#{"a" => [{str, "1"}], "b" => [{str, "A"}]}, #{"a" => [{str, "2"}, {str, "3"}], "b" => [{str, "B"}, {str, "C"}]}},
  X = corrstep( Lc, F0, F0 ),
  ?assertEqual( Y, X ).



-endif.