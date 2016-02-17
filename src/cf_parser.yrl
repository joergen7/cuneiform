%% -*- erlang -*-
%
% Cuneiform: A Functional Language for Large Scale Scientific Data Analysis
%
% Copyright 2013 Jörgen Brandt, Marc Bux, and Ulf Leser
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

Nonterminals 
  script stat assign exprlist expr binding assignlist sign query app cnd
  paramlist param inparamlist inparam namelist name defun lang bindinglist
  compoundexpr.

Terminals
  intlit strlit body bash beginif colon comma deftask else endif eq file in
  python r lbrace lparen lsquarebr ltag nil rbrace rparen rsquarebr rtag
  semicolon string then id.

  
Rootsymbol script.

script       -> stat        : '$1'.
script       -> stat script : combine( '$1', '$2' ).

stat         -> query  : {'$1', #{}, #{}}.
stat         -> assign : {[], '$1', #{}}.
stat         -> defun  : {[], #{}, '$1'}.

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

cnd          -> beginif compoundexpr then compoundexpr else
                compoundexpr endif                          : {cnd, get_line( '$1' ), '$2', '$4', '$6'}.

app          -> id lparen rparen                            : {app, get_line( '$1' ), 1, [mk_var( '$1' )], #{}}.
app          -> id lparen bindinglist rparen                : {app, get_line( '$1' ), 1, [mk_var( '$1' )], '$3'}.

binding      -> id colon compoundexpr : mk_binding( '$1', '$3' ).

bindinglist  -> binding                   : '$1'.
bindinglist  -> binding comma bindinglist : maps:merge( '$1', '$3' ).

sign         -> lparen paramlist colon rparen             : {sign, '$2', [], []}.
sign         -> lparen paramlist colon inparamlist rparen : {sign, '$2', [], '$4'}.

inparam      -> param                                          : '$1'.
inparam      -> lsquarebr namelist rsquarebr                   : {correl, '$2'}.

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


Erlang code.

-author( "Jörgen Brandt <brandjoe@hu-berlin.de>" ).

-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).
-endif.






combine( {Target1, Rho1, Global1}, {Target2, Rho2, Global2} ) ->
  {Target1++Target2, maps:merge( Rho1, Rho2 ), maps:merge( Global1, Global2 )}.

mk_var( {id, Line, Name} ) -> {var, Line, Name}.

mk_str( {intlit, Line, N} ) -> {str, Line, N};
mk_str( {strlit, Line, S} ) -> {str, Line, S}.

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
set_channel( E, _ ) -> error( {parser, cannot_set_channel_on_nonapp_expr, element( 1, E ), element( 2, E )} ).
  
mk_natlam( {deftask, Line, _}, {id, _, Name}, Sign, Block ) ->
  #{Name => [{lam, Line, Name, Sign, {natbody, Block}}]}.
  
mk_forlam( {deftask, Line, _}, {id, _, Name}, Sign, Lang, {body, _, Code} ) ->
  #{Name => [{lam, Line, Name, Sign, {forbody, Lang, Code}}]}.
  
  
-ifdef( TEST ).

% TEST INDEX

parse_test_() -> [nil_should_be_recognized(),
                  var_should_be_recognized(),
                  multi_element_compoundexpr_should_be_recognized(),
                  multiple_targets_should_be_joined(),
                  strlit_should_be_recognized(),
                  intlit_should_be_recognized(),
                  cnd_should_be_recognized(),
                  apply_should_be_recognized(),
                  call_should_be_recognized(),
                  assign_should_be_recognized(),
                  native_deftask_should_be_recognized(),
                  foreign_deftask_should_be_recognized(),
                  sign_with_inparam_should_be_recognized(),
                  param_should_be_recognized(),
                  task_correl_should_be_recognized(),
                  correl_inparam_should_be_recognized(),
                  comb_noreplace_should_be_recognized()
                 ].


% ACTUAL TEST IMPLEMENTATION

nil_should_be_recognized() ->
  ?_assertEqual( {[], #{}, #{}}, parse_string( "nil;" ) ).
  
var_should_be_recognized() ->
  ?_assertEqual( {[{var, 1, "blub"}], #{}, #{}}, parse_string( "blub;" ) ).
  
multi_element_compoundexpr_should_be_recognized() ->
  ?_assertEqual( {[{var, 1, "bla"}, {var, 1, "blub"}], #{}, #{}},
                 parse_string( "bla blub;" ) ).
  
multiple_targets_should_be_joined() ->
  ?_assertEqual( {[{var, 1, "bla"}, {var, 1, "blub"}], #{}, #{}},
                 parse_string( "bla; blub;" ) ).
  
strlit_should_be_recognized() ->
  ?_assertEqual( {[{str, 1, "bla"}], #{}, #{}}, parse_string( "'bla';" ) ).
  
intlit_should_be_recognized() ->
  ?_assertEqual( {[{str, 1, "-5"}], #{}, #{}}, parse_string( "-5;" ) ).
  
cnd_should_be_recognized() ->
  ?_assertEqual( {[{cnd, 1, [], [{str, 1, "bla"}], [{str, 1, "blub"}]}],
                  #{}, #{}},
                 parse_string( "if nil then 'bla' else 'blub' end;" ) ).
  
apply_should_be_recognized() ->
  [?_assertEqual( {[{app, 1, 1, [{var, 1, "f"}], #{}}], #{}, #{}},
                  parse_string( "apply( task: f );" ) ),
   ?_assertEqual( {[{app, 1, 1, [{var, 1, "f"}], #{"x" => [{var, 1, "x"}]}}],
                   #{}, #{}},
                  parse_string( "apply( task: f, x: x );" ) ),
   ?_assertEqual( {[{app, 1, 1, [{var, 1, "f"}], #{"x" => [{var, 1, "x"}],
                                                   "y" => [{str, 1, "y"}]}}],
                   #{}, #{}},
                  parse_string( "apply( task: f, x: x, y: 'y' );" ) ),
   ?_assertEqual( {[{app, 1, 1, [{var, 1, "f"}, {var, 1, "g"}],
                          #{"x" => [{var, 1, "x"}]}}], #{}, #{}},
                  parse_string( "apply( task: f g, x: x );" ) )].
  
call_should_be_recognized() ->
  [?_assertEqual( {[{app, 1, 1, [{var, 1, "f"}], #{}}], #{}, #{}},
                  parse_string( "f();" ) ),
   ?_assertEqual( {[{app, 1, 1, [{var, 1, "f"}], #{"x" => [{var, 1, "x"}]}}],
                   #{}, #{}}, parse_string( "f( x: x );" ) ),
   ?_assertEqual( {[{app, 1, 1, [{var, 1, "f"}],
                     #{"x" => [{var, 1, "x"}],
                       "y" => [{str, 1, "y"}]}}], #{}, #{}},
                  parse_string( "f( x: x, y: 'y' );" ) )].


assign_should_be_recognized() ->
  [?_assertEqual( {[], #{"x" => [{str, 1, "x"}]}, #{}}, parse_string( "x = 'x';" ) ),              
   ?_assertEqual( {[], #{"x" => [{app, 1, 1, [{var, 1, "f"}], #{}}], "y" => [{app, 1, 2, [{var, 1, "f"}], #{}}]}, #{}}, parse_string( "x y = f();" ) ),
   ?_assertError( {parser, cannot_set_channel_on_nonapp_expr, str, 1}, parse_string( "x y = 'A';" ) ),
   ?_assertError( {parser, nonvar_expr_left_of_eq, str, 1}, parse_string( "'a' = 'A';" ) )].
   
native_deftask_should_be_recognized() ->
  ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                           {sign, [{param, {name, "out", false}, false}],
                                                  [], []},
                                           {natbody, #{"out" => [{str, 1, "A"}]}}}]}},
                 parse_string( "deftask f( out : ) { out = 'A'; }" ) ).
                 
foreign_deftask_should_be_recognized() ->
  [?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [], []},
                                            {forbody, bash, "out=A"}}]}},
                  parse_string( "deftask f( out : )in bash *{out=A}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [], []},
                                            {forbody, r, "out=\"A\""}}]}},
                  parse_string( "deftask f( out : )in R *{out=\"A\"}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [], []},
                                            {forbody, matlab, "out=\"A\""}}]}},
                  parse_string( "deftask f( out : )in matlab *{out=\"A\"}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [], []},
                                            {forbody, octave, "out=\"A\""}}]}},
                  parse_string( "deftask f( out : )in octave *{out=\"A\"}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [], []},
                                            {forbody, perl, ""}}]}},
                  parse_string( "deftask f( out : )in perl *{}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [], []},
                                            {forbody, python, ""}}]}},
                  parse_string( "deftask f( out : )in python *{}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [], []},
                                            {forbody, lisp, "(defparameter out \"A\")"}}]}},
                  parse_string( "deftask f( out : )in lisp *{(defparameter out \"A\")}*" ) )].
                  
sign_with_inparam_should_be_recognized() ->
  [?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [],
                                                   [{param, {name, "inp", false}, false}]},
                                            {forbody, lisp, "(defparameter out \"A\")"}}]}},
                  parse_string( "deftask f( out : inp )in lisp *{(defparameter out \"A\")}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [],
                                                   [{param, {name, "a", false}, false},
                                                    {param, {name, "b", false}, false}]},
                                            {forbody, lisp, "(defparameter out \"A\")"}}]}},
                  parse_string( "deftask f( out : a b )in lisp *{(defparameter out \"A\")}*" ) )].
                  
param_should_be_recognized() ->
  [?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [],
                                                   [{param, {name, "inp", false}, false}]},
                                            {forbody, bash, "blub"}}]}},
                  parse_string( "deftask f( out( String ) : inp( String ) )in bash *{blub}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", true}, false}],
                                                   [],
                                                   [{param, {name, "inp", true}, false}]},
                                            {forbody, bash, "blub"}}]}},
                  parse_string( "deftask f( out( File ) : inp( File ) )in bash *{blub}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, true}],
                                                   [],
                                                   [{param, {name, "inp", false}, true}]},
                                            {forbody, bash, "blub"}}]}},
                  parse_string( "deftask f( <out> : <inp> )in bash *{blub}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, true}],
                                                   [],
                                                   [{param, {name, "inp", false}, true}]},
                                            {forbody, bash, "blub"}}]}},
                  parse_string( "deftask f( <out( String )> : <inp( String )> )in bash *{blub}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", true}, true}],
                                                   [],
                                                   [{param, {name, "inp", true}, true}]},
                                            {forbody, bash, "blub"}}]}},
                  parse_string( "deftask f( <out( File )> : <inp( File )> )in bash *{blub}*" ) )].
  
task_correl_should_be_recognized() ->
  [?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [{name, "a", false}],
                                                   [{param, {name, "b", false}, false}]},
                                            {forbody, bash, "blub"}}]}},
                  parse_string( "deftask f( out : [task a] b )in bash *{blub}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [{name, "a", false},
                                                    {name, "b", false}],
                                                   []},
                                            {forbody, bash, "blub"}}]}},
                  parse_string( "deftask f( out : [task a b] )in bash *{blub}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [{name, "a", false}],
                                                   [{param, {name, "b", false}, false}]},
                                            {forbody, bash, "blub"}}]}},
                  parse_string( "deftask f( out : [task a( String )] b )in bash *{blub}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [{name, "a", false},
                                                    {name, "b", false}],
                                                   []},
                                            {forbody, bash, "blub"}}]}},
                  parse_string( "deftask f( out : [task a( String ) b( String )] )in bash *{blub}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [{name, "a", true}],
                                                   [{param, {name, "b", false}, false}]},
                                            {forbody, bash, "blub"}}]}},
                  parse_string( "deftask f( out : [task a( File )] b )in bash *{blub}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [{name, "a", true},
                                                    {name, "b", true}],
                                                   []},
                                            {forbody, bash, "blub"}}]}},
                  parse_string( "deftask f( out : [task a( File ) b( File )] )in bash *{blub}*" ) )].    
                  
correl_inparam_should_be_recognized() ->
  [?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [],
                                                   [{correl, [{name, "a", true},
                                                              {name, "b", true}]}]},
                                            {forbody, bash, "blub"}}]}},
                  parse_string( "deftask f( out : [a( File ) b( File )] )in bash *{blub}*" ) ),
   ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                            {sign, [{param, {name, "out", false}, false}],
                                                   [],
                                                   [{correl, [{name, "a", true},
                                                              {name, "b", true}]},
                                                    {param, {name, "c", false}, false}]},
                                            {forbody, bash, "blub"}}]}},
                  parse_string( "deftask f( out : [a( File ) b( File )] c )in bash *{blub}*" ) )].

comb_noreplace_should_be_recognized() ->
  ?_assertEqual( {[], #{}, #{"f" => [{lam, 1, "f",
                                           {sign, [{param, {name, "out", false}, false}],
                                                  [],
                                                  [{comb, cnr, {name, "x", true}, ["a", "b", "c"]}]},
                                           {forbody, bash, "blub"}}]}},
                 parse_string( "deftask f( out : {comb noreplace x( File ): a b c} )in bash *{blub}*" ) ).







parse_string( S ) ->
  {ok, TokenList, _} = cuneiform_lexer:string( S ),
  {ok, ParseTree} = cuneiform_parser:parse( TokenList ),
  ParseTree.



-endif.


