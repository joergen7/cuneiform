%% -*- erlang -*-

-module( cf_sem ).
-author( "Jörgen Brandt <brandjoe@hu-berlin.de>" ).

-export( [eval/2] ).

-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).
-endif.



%% =============================================================================
%% Abstract Syntax
%% =============================================================================

%% Expression %% ===============================================================

-type expr()    :: str() | var() | select() | cnd() | app().                    % (1)
-type str()     :: {str, Line::pos_integer(), S::string()}.                        % (2)
-type var()     :: {var, Line::pos_integer(), N::string()}.                        % (3)
-type select()  :: {select, Line::pos_integer(), C::pos_integer(), U::fut()}.      % (4)
-type fut()     :: {fut, Line::pos_integer(), Name::string(), R::pos_integer(),       % (5)
                         Lp::[boolean()]}.
-type cnd()     :: {cnd, Line::pos_integer(),                                      % (6)
                         Xc::[expr()], Xt::[expr()], Xe::[expr()]}.
-type app()     :: {app, Line::pos_integer(), C::pos_integer(),                    % (7)
                         Lambda::lam() | var(), Fa::#{string() => [expr()]}}.

%% Lambda %% ===================================================================

-type lam()     :: {lam, Line::pos_integer(), Name::string(), S::sign(), B::body()}.  % (8)

% Task Signature
-type sign()    :: {sign, Lo::[param()], Li::[inparam()]}.                      % (9)
-type param()   :: {param, N::string(), Pl::boolean()}.                         % (10)
-type inparam() :: param() | correl().                                          % (11)
-type correl()  :: {correl, Lc::[string()]}.                                    % (12)

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
                    Omega :: #{select() => [expr()]}}.


%% =============================================================================
%% Predicates
%% =============================================================================

%% Finality %% =================================================================

-spec pfinal( X ) -> boolean()                                                  % (19)
when X :: #{string() => [expr()]} | [expr()] | expr().

pfinal( F ) when is_map( F )   -> pfinal( maps:values( F ) );                   % (20)
pfinal( L ) when is_list( L )  -> lists:all( fun pfinal/1, L );                 % (21,22)
pfinal( {str, _, _S} )         -> true;                                         % (23)
pfinal( _T )                   -> false.

%% Singularity %% ==============================================================

-spec psing( A::app() ) -> boolean().                                           % (24)

psing( {app, _, _C, {lam, _, _, {sign, _Lo, Li}, _B}, Fa} ) ->                  % (25)
  psing_argpair( {Li, Fa} ).

-spec psing_argpair( Z::argpair() ) -> boolean().                               % (26)

psing_argpair( {[], _F} )                         -> true;                      % (27)
psing_argpair( {[{param, _N, Pl}|T], F} ) when Pl -> psing_argpair( {T, F} );   % (28)
psing_argpair( {[{param, N, _Pl}|T], F} ) ->                                    % (29)
  case length( maps:get( N, F ) ) of
    1 -> psing_argpair( {T, F} );
    _ -> false
  end;
psing_argpair( _Z ) -> false.

%% Enumerability %% ============================================================

-spec pen( X::expr()|[expr()] ) -> boolean().                                   % (30)

pen( X )when is_list( X ) -> lists:all( fun pen/1, X );                         % (31,32)
pen( {str, _, _S} ) -> true;                                                    % (33)
pen( {cnd, _, _Xc, Xt, Xe} )when length( Xt ) =:= 1, length( Xe ) =:= 1 ->      % (34)
  pen( Xt ) andalso pen( Xe );
pen( X={app, _, C, {lam, _, _, {sign, Lo, _Li}, _B}, _Fb} ) ->                  % (35)
  case psing( X ) of
    false -> false;
    true ->
      {param, _N, Pl} = lists:nth( C, Lo ),
      not Pl
  end;
pen( {select, _, C, {fut, _, _, _R, Lp}} ) -> not lists:nth( C, Lp );                 % (36)
pen( _T ) -> false.

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
step( X={str, _, _S}, _Theta ) -> [X];                                          % (45)

% Variable
step( {var, _, N}, {Rho, _Mu, _Gamma, _Omega} ) ->                              % (46)
  maps:get( N, Rho );
  
% Future Channel Selection
step( S={select, _, _C, _Fut}, {_Rho, _Mu, _Gamma, Omega} ) ->                  % (47,48)
  maps:get( S, Omega, [S] );

% Conditional
step( {cnd, _, [], _Xt, Xe}, _Theta ) -> Xe;                                    % (49)
step( {cnd, Line, Xc=[_|_], Xt, Xe}, Theta ) ->
  case pfinal( Xc ) of
    false -> [{cnd, Line, step( Xc, Theta ), Xt, Xe}];                          % (50)
    true  -> Xt                                                                 % (51)
  end;
  
% Application
step( {app, Line, C, {var, _, N}, Fa}, {_Rho, _Mu, Gamma, _Omega} ) ->
  [{app, Line, C, maps:get( N, Gamma ), Fa}];
  
step( X={app, AppLine, C, Lambda={lam, LamLine, LamName, S={sign, Lo, _Li}, B}, Fa},
      Theta={_Rho, Mu, Gamma, Omega} ) ->
  case psing( X ) of
    false -> enum_app( {app, AppLine, C, Lambda, step_assoc( Fa, Theta )} );    % (53)
    true  ->
      case B of
        {forbody, _L, _Z} ->
          case pfinal( Fa ) of
            false -> [{app, AppLine, C, Lambda, step_assoc( Fa, Theta )}];      % (54)
            true  -> [{select, AppLine, C, apply( Mu, [X] )}]                   % (55)
          end;
        {natbody, Fb} ->
          {param, K, Pl} = lists:nth( C, Lo ),
          V0 = maps:get( K, Fb ),
          V1 = step( V0, {maps:merge( Fb, Fa ), Mu, Gamma, Omega} ),
          case pfinal( V1 ) of
            false -> [{app, AppLine, C, {lam, LamLine, LamName, S,              % (56)
                                              {natbody, Fb#{ K => V1}}}, Fa}];
            true  ->
              case Pl orelse length( V1 ) =:= 1 of
                true  -> V1;                                                    % (57)
                false -> error( output_sign_mismatch )
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
  lists:flatmap( fun( {Li, F} ) -> estep( Li, F ) end, Z ).

-spec estep( Li::[inparam()], F::#{string() => [expr()]} ) -> [argpair()].      % (66)

estep( [], F ) -> [{[], F}];                                                    % (67)
estep( [H={param, _N, Pl}|T], F ) when Pl -> aug( estep( T, F ), H );           % (68)
estep( L=[H={param, N, _Pl}|T], F ) ->
  V = maps:get( N, F ),
  case pen( V ) of
    false -> [{L, F}];                                                          % (69)
    true  ->
      case length( V ) of
        1 -> aug( estep( T, F ), H );                                           % (70)
        _ -> [{L, maps:put( N, [X], F )} || X <- V]                             % (71)
      end
  end;
estep( L=[H={correl, Lc}|T], F ) when length( Lc ) > 1 ->
  Pen = pen( [maps:get( N, F ) || N <- Lc] ),
  case Pen of
    false -> [{L, F}];                                                          % (72)
    true  ->
      Z = corrstep( Lc, F, F ),
      aug( [{T, G} || G <- Z], H )                                               % (73)
  end.
      
  

%% Augmentation %%

-spec aug( Z::[argpair()], A::inparam() ) -> [argpair()].                       % (74)

aug( Z, A ) -> [aug_argpair( Y, A ) || Y <- Z].                                 % (75)

-spec aug_argpair( Y::argpair(), A::inparam() ) -> argpair().                   % (76)

aug_argpair( {L0, F}, A={param, _N, _Pl} ) -> {[A|L0], F};                      % (77)
aug_argpair( {L0, F}, {correl, Lc} ) ->                                         % (78)
  L1 = [{param, N, false} || N <- Lc],
  {L1++L0, F}.

%% Correlation %%

-spec corrstep( Lc, Facc, F0 ) -> [#{string() => [expr()]}]                     % (79)
when Lc   :: [string()],
     Facc :: #{string() => [expr()]},
     F0   :: #{string() => [expr()]}.

corrstep( [], Facc, F0 ) -> [Facc, F0];                                         % (80)
corrstep( [H|T], Facc, F0 ) ->
  case maps:get( H, F0 ) of
    []    -> [];                                                                % (81)
    [A|B] -> corrstep( T, Facc#{H => [A]}, F0#{H => B})                         % (82)
  end.




%% =============================================================================
%% Unit Tests
%% =============================================================================

-ifdef( TEST ).

%% The enum Function %%

enum_app_without_app_does_nothing_test() ->
  S = {sign, [{param, "out", false}], []},
  B = {forbody, bash, "shalala"},
  Lam = {lam, 1, "f", S, B},
  App = {app, 2, 1, Lam, #{}},
  ?assertEqual( [App], enum_app( App ) ).

%% Enumeration Rules %%

%% Augmentation %%

can_aug_argpair_with_param_test() ->
  L0 = [{param, "b", false}, {param, "c", false}],
  F = #{"a" => [{str, 1, "1"}], "b" => [{str, 2, "2"}], "c" => [{str, 3, "3"}]},
  Pair = {L0, F},
  I = {param, "a", false},
  L1 = [I|L0],
  ?assertEqual( {L1, F}, aug_argpair( Pair, I ) ).

can_aug_argpair_with_correl_test() ->
  L0 = [{param, "b", false}, {param, "c", false}],
  F = #{"a1" => [{str, 1, "11"}],
        "a2" => [{str, 2, "12"}],
        "b" => [{str, 3, "2"}],
        "c" => [{str, 4, "3"}]},
  Pair = {L0, F},
  I = {correl, ["a1", "a2"]},
  L1 = [{param, "a1", false}, {param, "a2", false}|L0],
  ?assertEqual( {L1, F}, aug_argpair( Pair, I ) ).

can_augment_empty_argpairlist_with_param_test() ->
  F1 = #{"a" => [{str, 1, "x1"}]},
  F2 = #{"a" => [{str, 2, "y1"}]},
  PairList = [{[], F1}, {[], F2}],
  I = {param, "a", false},
  L1 = [{param, "a", false}],
  ?assertEqual( [{L1, F1}, {L1, F2}], aug( PairList, I ) ).

can_augment_argpairlist_with_param_test() ->
  L0 = [{param, "b", false}, {param, "c", false}],
  F1 = #{"a" => [{str, 1, "x1"}], "b" => [{str, 2, "x2"}], "c" => [{str, 3, "x3"}]},
  F2 = #{"a" => [{str, 4, "y1"}], "b" => [{str, 5, "y2"}], "c" => [{str, 6, "y3"}]},
  PairList = [{L0, F1}, {L0, F2}],
  I = {param, "a", false},
  L1 = [{param, "a", false}|L0],
  ?assertEqual( [{L1, F1}, {L1, F2}], aug( PairList, I ) ).

can_augment_argpairlist_with_correl_test() ->
  L0 = [{param, "b", false}, {param, "c", false}],
  F1 = #{"a1" => [{str, 1, "x11"}],
         "a2" => [{str, 2, "x12"}],
         "b" => [{str, 3, "x2"}],
         "c" => [{str, 4, "x3"}]},
  F2 = #{"a1" => [{str, 5, "y11"}],
         "a2" => [{str, 6, "y12"}],
         "b" => [{str, 7, "y2"}],
         "c" => [{str, 8, "y3"}]},
  PairList = [{L0, F1}, {L0, F2}],
  I = {correl, ["a1", "a2"]},
  L1 = [{param, "a1", false}, {param, "a2", false}|L0],
  ?assertEqual( [{L1, F1}, {L1, F2}], aug( PairList, I ) ).

%% Correlation %%





-endif.