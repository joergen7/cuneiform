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

%% =============================================================================
%% Symbol Declaration
%% =============================================================================

Nonterminals
  script stat assign exprlist expr binding assignlist sign query app cnd
  paramlist param inparamlist inparam namelist name defun lang bindinglist
  compoundexpr.

Terminals
  intlit strlit body bash beginif colon comma deftask else endif eq file in
  python r lbrace lparen lsquarebr ltag nil rbrace rparen rsquarebr rtag
  semicolon string then id.


%% =============================================================================
%% Syntax Definition
%% =============================================================================

Rootsymbol script.


script       -> stat        : '$1'.
script       -> stat script : combine( '$1', '$2' ).

stat         -> query  : {'$1', #{}, #{}}.
stat         -> assign : {undef, '$1', #{}}.
stat         -> defun  : {undef, #{}, '$1'}.

query        -> compoundexpr semicolon : '$1'.

assign       -> exprlist eq compoundexpr semicolon : mk_assign( '$1', '$3', 1 ).

assignlist   -> assign            : '$1'.
assignlist   -> assign assignlist : maps:merge( '$1', '$2' ).

defun        -> deftask id sign lbrace assignlist rbrace : mk_natlam( '$1', '$2', '$3', '$5' ).
defun        -> deftask id sign in lang body             : mk_forlam( '$1', '$2', '$3', '$5', '$6' ).

lang         -> bash   : bash.
lang         -> python : python.
lang         -> r      : r.

compoundexpr -> nil      : [].
compoundexpr -> exprlist : '$1'.

expr         -> intlit : mk_str( '$1' ).
expr         -> strlit : mk_str( '$1' ).
expr         -> id     : mk_var( '$1' ).
expr         -> cnd    : '$1'.
expr         -> app    : '$1'.

exprlist     -> expr          : ['$1'].
exprlist     -> expr exprlist : ['$1'|'$2'].

cnd          -> beginif compoundexpr then compoundexpr
                else compoundexpr endif                : {cnd, get_line( '$1' ), '$2', '$4', '$6'}.

app          -> id lparen rparen             : {app, get_line( '$1' ), 1, mk_var( '$1' ), #{}}.
app          -> id lparen bindinglist rparen : {app, get_line( '$1' ), 1, mk_var( '$1' ), '$3'}.

binding      -> id colon compoundexpr : mk_binding( '$1', '$3' ).

bindinglist  -> binding                   : '$1'.
bindinglist  -> binding comma bindinglist : maps:merge( '$1', '$3' ).

sign         -> lparen paramlist colon rparen             : {sign, '$2', []}.
sign         -> lparen paramlist colon inparamlist rparen : {sign, '$2', '$4'}.

inparam      -> param                        : '$1'.
inparam      -> lsquarebr namelist rsquarebr : {correl, '$2'}.

inparamlist  -> inparam             : ['$1'].
inparamlist  -> inparam inparamlist : ['$1'|'$2'].

param        -> name           : {param, '$1', false}.
param        -> ltag name rtag : {param, '$2', true}.

paramlist    -> param           : ['$1'].
paramlist    -> param paramlist : ['$1'|'$2'].

name         -> id                      : {name, get_name( '$1' ), false}.
name         -> id lparen string rparen : {name, get_name( '$1' ), false}.
name         -> id lparen file rparen   : {name, get_name( '$1' ), true}.

namelist     -> name          : ['$1'].
namelist     -> name namelist : ['$1'|'$2'].

%% =============================================================================
%% Erlang Code
%% =============================================================================

Erlang code.

-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).

-export( [string/1] ).

-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).
-endif.

string( S ) ->
  % scan
  case cf_scan:string( S ) of
    {error, ScanErrorInfo, _} -> error( ScanErrorInfo );
    {ok, TokenLst, _}         ->
      % parse
      case cf_parse:parse( TokenLst ) of
        {error, ParseErrorInfo} -> error( ParseErrorInfo );
        {ok, Triple}            -> Triple
      end
  end.


combine( {Target1, Rho1, Global1}, {Target2, Rho2, Global2} ) ->
  case Target1 of
    undef -> {Target2, maps:merge( Rho1, Rho2 ), maps:merge( Global1, Global2 )};
    _     ->
      case Target2 of
        undef -> {undef, maps:merge( Rho1, Rho2 ), maps:merge( Global1, Global2 )};
        _     -> {Target1++Target2, maps:merge( Rho1, Rho2 ), maps:merge( Global1, Global2 )}
      end
  end.

mk_var( {id, Line, Name} ) -> {var, Line, Name}.

mk_str( {intlit, _Line, N} ) -> {str, N};
mk_str( {strlit, _Line, S} ) -> {str, S}.

get_line( {_, Line, _} ) -> Line.

get_name( {id, _Line, Name} ) -> Name.

mk_binding( {id, _, Name}, ExprList ) ->
  #{Name => ExprList}.

mk_assign( [], _ExprList, _Channel ) -> #{};

mk_assign( [{var, _Line, Name}|Rest], ExprList, Channel ) ->
  Rho = mk_assign( Rest, ExprList, Channel+1 ),
  Value = lists:flatmap( fun( E ) -> set_channel( E, Channel ) end, ExprList ),
  Rho#{Name => Value};

mk_assign( [E|_Rest], _ExprList, _Channel ) ->
  error( {parser, nonvar_expr_left_of_eq, element( 1, E ), element( 2, E )} ).


set_channel( {app, Line, _Channel, LamList, Binding}, N ) -> [{app, Line, N, LamList, Binding}];
set_channel( E, 1 ) -> [E];
set_channel( E, _ ) -> error( {parser, nonapp_expr, element( 1, E ), element( 2, E )} ).

mk_natlam( {deftask, Line, _}, {id, _, Name}, Sign, Block ) ->
  #{Name => {lam, Line, Name, Sign, {natbody, Block}}}.

mk_forlam( {deftask, Line, _}, {id, _, Name}, Sign, Lang, {body, _, Code} ) ->
  #{Name => {lam, Line, Name, Sign, {forbody, Lang, Code}}}.


%% =============================================================================
%% Unit Tests
%% =============================================================================

-ifdef( TEST ).

nil_should_be_recognized_test() ->
  ?assertEqual( {[], #{}, #{}}, string( "nil;" ) ).

var_should_be_recognized_test() ->
  ?assertEqual( {[{var, 1, "blub"}], #{}, #{}}, string( "blub;" ) ).

multi_element_compoundexpr_should_be_recognized_test() ->
  ?assertEqual( {[{var, 1, "bla"}, {var, 1, "blub"}], #{}, #{}},
                 string( "bla blub;" ) ).

multiple_queries_should_be_joined_test() ->
  ?assertEqual( {[{var, 1, "bla"}, {var, 1, "blub"}], #{}, #{}},
                 string( "bla; blub;" ) ).

strlit_should_be_recognized_test() ->
  ?assertEqual( {[{str, "bla"}], #{}, #{}}, string( "\"bla\";" ) ).

intlit_should_be_recognized_test() ->
  ?assertEqual( {[{str, "-5"}], #{}, #{}}, string( "-5;" ) ).

cnd_should_be_recognized_test() ->
  ?assertEqual( {[{cnd, 1, [], [{str, "bla"}], [{str, "blub"}]}],
                  #{}, #{}},
                 string( "if nil then \"bla\" else \"blub\" end;" ) ).

app_should_be_recognized_test() ->
  [?assertEqual( {[{app, 1, 1, {var, 1, "f"}, #{}}], #{}, #{}},
                  string( "f();" ) ),
   ?assertEqual( {[{app, 1, 1, {var, 1, "f"}, #{"x" => [{var, 1, "x"}]}}],
                   #{}, #{}}, string( "f( x: x );" ) ),
   ?assertEqual( {[{app, 1, 1, {var, 1, "f"},
                     #{"x" => [{var, 1, "x"}],
                       "y" => [{str, "y"}]}}], #{}, #{}},
                  string( "f( x: x, y: \"y\" );" ) )].


assign_should_be_recognized_test() ->
  [?assertEqual( {undef, #{"x" => [{str, "x"}]}, #{}}, string( "x = \"x\";" ) ),
   ?assertEqual( {undef, #{"x" => [{app, 1, 1, {var, 1, "f"}, #{}}], "y" => [{app, 1, 2, {var, 1, "f"}, #{}}]}, #{}}, string( "x y = f();" ) ),
   ?assertError( {parser, nonapp_expr, str, "A"}, string( "x y = \"A\";" ) ),
   ?assertError( {parser, nonvar_expr_left_of_eq, str, "a"}, string( "\"a\" = \"A\";" ) )].

native_deftask_should_be_recognized_test() ->
  ?assertEqual( {undef, #{}, #{"f" => {lam, 1, "f",
                                           {sign, [{param, {name, "out", false}, false}],
                                                  []},
                                           {natbody, #{"out" => [{str, "A"}]}}}}},
                 string( "deftask f( out : ) { out = \"A\"; }" ) ).

foreign_deftask_should_be_recognized_test() ->
  [?assertEqual( {undef, #{}, #{"f" => {lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   []},
                                            {forbody, bash, "out=A"}}}},
                  string( "deftask f( out : )in bash *{out=A}*" ) ),
   ?assertEqual( {undef, #{}, #{"f" => {lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   []},
                                            {forbody, r, "out=\"A\""}}}},
                  string( "deftask f( out : )in R *{out=\"A\"}*" ) ),
   ?assertEqual( {undef, #{}, #{"f" => {lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   []},
                                            {forbody, python, ""}}}},
                  string( "deftask f( out : )in python *{}*" ) )].

sign_with_inparam_should_be_recognized_test() ->
  [?assertEqual( {undef, #{}, #{"f" => {lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [{param, {name, "inp", false}, false}]},
                                            {forbody, python, "(defparameter out \"A\")"}}}},
                  string( "deftask f( out : inp )in python *{(defparameter out \"A\")}*" ) ),
   ?assertEqual( {undef, #{}, #{"f" => {lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [{param, {name, "a", false}, false},
                                                    {param, {name, "b", false}, false}]},
                                            {forbody, python, "(defparameter out \"A\")"}}}},
                  string( "deftask f( out : a b )in python *{(defparameter out \"A\")}*" ) )].

param_should_be_recognized_test() ->
  [?assertEqual( {undef, #{}, #{"f" => {lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [{param, {name, "inp", false}, false}]},
                                            {forbody, bash, "blub"}}}},
                  string( "deftask f( out( String ) : inp( String ) )in bash *{blub}*" ) ),
   ?assertEqual( {undef, #{}, #{"f" => {lam, 1, "f",
                                            {sign, [{param, {name, "out", true}, false}],
                                                   [{param, {name, "inp", true}, false}]},
                                            {forbody, bash, "blub"}}}},
                  string( "deftask f( out( File ) : inp( File ) )in bash *{blub}*" ) ),
   ?assertEqual( {undef, #{}, #{"f" => {lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, true}],
                                                   [{param, {name, "inp", false}, true}]},
                                            {forbody, bash, "blub"}}}},
                  string( "deftask f( <out> : <inp> )in bash *{blub}*" ) ),
   ?assertEqual( {undef, #{}, #{"f" => {lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, true}],
                                                   [{param, {name, "inp", false}, true}]},
                                            {forbody, bash, "blub"}}}},
                  string( "deftask f( <out( String )> : <inp( String )> )in bash *{blub}*" ) ),
   ?assertEqual( {undef, #{}, #{"f" => {lam, 1, "f",
                                            {sign, [{param, {name, "out", true}, true}],
                                                   [{param, {name, "inp", true}, true}]},
                                            {forbody, bash, "blub"}}}},
                  string( "deftask f( <out( File )> : <inp( File )> )in bash *{blub}*" ) )].

correl_inparam_should_be_recognized_test() ->
  [?assertEqual( {undef, #{}, #{"f" => {lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [{correl, [{name, "a", true},
                                                              {name, "b", true}]}]},
                                            {forbody, bash, "blub"}}}},
                  string( "deftask f( out : [a( File ) b( File )] )in bash *{blub}*" ) ),
   ?assertEqual( {undef, #{}, #{"f" => {lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [{correl, [{name, "a", true},
                                                              {name, "b", true}]},
                                                    {param, {name, "c", false}, false}]},
                                            {forbody, bash, "blub"}}}},
                  string( "deftask f( out : [a( File ) b( File )] c )in bash *{blub}*" ) )].

-endif.


