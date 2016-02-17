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

Definitions.

BASH      = [Bb]ash
PYTHON    = [Pp]ython
R         = [Rr]

BEGINIF   = if
COLON     = :
COMMA     = ,
DEFTASK   = deftask
ELSE      = else
ENDIF     = end
EQ        = =
FILE      = File
IN        = in
LBRACE    = \{
LPAREN    = \(
LSQUAREBR = \[
LTAG      = <
NIL       = nil
RBRACE    = \}
RPAREN    = \)
RSQUAREBR = \]
RTAG      = >
SEMICOLON = ;
STRING    = String
THEN      = then

ID        = [A-Za-z][A-Za-z0-9.\-_]*
INTLIT    = -?([0-9]|[1-9][0-9]*)
STRLIT    = "[^"]*"
BODY      = \*\{([^}]|\}+[^}*])*\}+\*


COMMENT1  = (#|//|%).*
COMMENT2  = /\*([^*]|\*+[^/*])*\*+/
WS        = [\000-\s]





Rules.

{BASH}      : {token, {bash, TokenLine, TokenChars}}.
{PYTHON}    : {token, {python, TokenLine, TokenChars}}.
{R}         : {token, {r, TokenLine, TokenChars}}.

{INTLIT}    : {token, {intlit, TokenLine, TokenChars}}.
{STRLIT}    : {token, {strlit, TokenLine, trim_strlit( TokenChars )}}.
{BODY}      : {token, {body, TokenLine, trim_body( TokenChars )}}.
{BEGINIF}   : {token, {beginif, TokenLine, TokenChars}}.
{COLON}     : {token, {colon, TokenLine, TokenChars}}.
{COMMA}     : {token, {comma, TokenLine, TokenChars}}.
{DEFTASK}   : {token, {deftask, TokenLine, TokenChars}}.
{ELSE}      : {token, {else, TokenLine, TokenChars}}.
{ENDIF}     : {token, {endif, TokenLine, TokenChars}}.
{EQ}        : {token, {eq, TokenLine, TokenChars}}.
{FILE}      : {token, {file, TokenLine, TokenChars}}.
{IN}        : {token, {in, TokenLine, TokenChars}}.
{LBRACE}    : {token, {lbrace, TokenLine, TokenChars}}.
{LPAREN}    : {token, {lparen, TokenLine, TokenChars}}.
{LSQUAREBR} : {token, {lsquarebr, TokenLine, TokenChars}}.
{LTAG}      : {token, {ltag, TokenLine, TokenChars}}.
{NIL}       : {token, {nil, TokenLine, TokenChars}}.
{RBRACE}    : {token, {rbrace, TokenLine, TokenChars}}.
{RPAREN}    : {token, {rparen, TokenLine, TokenChars}}.
{RSQUAREBR} : {token, {rsquarebr, TokenLine, TokenChars}}.
{RTAG}      : {token, {rtag, TokenLine, TokenChars}}.
{SEMICOLON} : {token, {semicolon, TokenLine, TokenChars}}.
{STRING}    : {token, {string, TokenLine, TokenChars}}.
{THEN}      : {token, {then, TokenLine, TokenChars}}.

{ID}        : {token, {id, TokenLine, TokenChars}}.

{COMMENT1}  : skip_token.
{COMMENT2}  : skip_token.
{WS}        : skip_token.






Erlang code.

-author( "Jörgen Brandt <brandjoe@hu-berlin.de>" ).

-export( [yyrev/2] ).

-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).
-endif.

trim_body( S ) ->
  string:substr( S, 3, length( S )-4 ).
  
trim_strlit( S ) ->
  string:substr( S, 2, length( S )-2 ).
  
  
-ifdef( TEST ).


% TEST INDEX

token_test_() -> [
                  bash_style_comment_should_be_recognized(),
                  c_style_comment_should_be_recognized(),
                  erlang_style_comment_should_be_recognized(),
                  multiline_comment_should_be_recognized(),
                  apply_should_be_recognized(),
                  bash_should_be_recognized(),
                  beginif_should_be_recognized(),
                  colon_should_be_recognized(),
                  comb_should_be_recognized(),
                  deftask_should_be_recognized(),
                  else_should_be_recognized(),
                  endif_should_be_recognized(),
                  eq_should_be_recognized(),
                  file_should_be_recognized(),
                  in_should_be_recognized(),
                  id_should_be_recognized(),
                  lisp_should_be_recognized(),
                  matlab_should_be_recognized(),
                  octave_should_be_recognized(),
                  perl_should_be_recognized(),
                  python_should_be_recognized(),
                  r_should_be_recognized(),
                  lbrace_should_be_recognized(),
                  lparen_should_be_recognized(),
                  lsquarebr_should_be_recognized(),
                  ltag_should_be_recognized(),
                  nil_should_be_recognized(),
                  noreplace_should_be_recognized(),
                  rbrace_should_be_recognized(),
                  rparen_should_be_recognized(),
                  rsquarebr_should_be_recognized(),
                  rtag_should_be_recognized(),
                  semicolon_should_be_recognized(),
                  string_should_be_recognized(),
                  task_should_be_recognized(),
                  then_should_be_recognized(),
                  intlit_should_be_recognized(),
                  body_should_be_recognized(),
                  strlit_should_be_recognized()
                 ].




% ACTUAL TEST IMPLEMENTATION

bash_style_comment_should_be_recognized() ->
  ?_assertEqual( {ok, [], 1}, string( "#this is a comment" ) ).
  
c_style_comment_should_be_recognized() ->
  ?_assertEqual( {ok, [], 1}, string( "//this is a comment" ) ).
  
erlang_style_comment_should_be_recognized() ->
  ?_assertEqual( {ok, [], 1}, string( "%this is a comment" ) ).
  
  
multiline_comment_should_be_recognized() ->
  [?_assertEqual( {ok, [], 1}, string( "/**/" ) ),
   ?_assertEqual( {ok, [], 1}, string( "/*x*/" ) ),
   ?_assertEqual( {ok, [], 1}, string( "/*xy*/" ) ),
   ?_assertEqual( {ok, [], 2}, string( "/*x\ny*/" ) )].
  
apply_should_be_recognized() ->
  ?_assertEqual( {ok, [{apply, 1, "apply"}], 1}, string( "apply" ) ).
  
bash_should_be_recognized() ->
  ?_assertEqual( {ok, [{bash, 1, "bash"}], 1}, string( "bash" ) ).
  
beginif_should_be_recognized() ->
  ?_assertEqual( {ok, [{beginif, 1, "if"}], 1}, string( "if" ) ).
  
colon_should_be_recognized() ->
  ?_assertEqual( {ok, [{colon, 1, ":"}], 1}, string( ":" ) ).
  
comb_should_be_recognized() ->
  ?_assertEqual( {ok, [{comb, 1, "comb"}], 1}, string( "comb" ) ).

deftask_should_be_recognized() ->
  ?_assertEqual( {ok, [{deftask, 1, "deftask"}], 1}, string( "deftask" ) ).

else_should_be_recognized() ->
  ?_assertEqual( {ok, [{else, 1, "else"}], 1}, string( "else" ) ).

endif_should_be_recognized() ->
  ?_assertEqual( {ok, [{endif, 1, "end"}], 1}, string( "end" ) ).

eq_should_be_recognized() ->
  ?_assertEqual( {ok, [{eq, 1, "="}], 1}, string( "=" ) ).

file_should_be_recognized() ->
  ?_assertEqual( {ok, [{file, 1, "File"}], 1}, string( "File" ) ).

in_should_be_recognized() ->
  ?_assertEqual( {ok, [{in, 1, "in"}], 1}, string( "in" ) ).
  
id_should_be_recognized() ->
  [?_assertEqual( {ok, [{id, 1, "inp"}], 1}, string( "inp" ) ),
   ?_assertEqual( {ok, [{id, 1, "a0"}], 1}, string( "a0" ) ),
   ?_assertEqual( {ok, [{id, 1, "a9"}], 1}, string( "a9" ) ),
   ?_assertEqual( {ok, [{id, 1, "a."}], 1}, string( "a." ) ),
   ?_assertEqual( {ok, [{id, 1, "a-"}], 1}, string( "a-" ) ),
   ?_assertEqual( {ok, [{id, 1, "a_"}], 1}, string( "a_" ) ),
   ?_assertEqual( {ok, [{id, 1, "a+"}], 1}, string( "a+" ) ),
   ?_assertEqual( {ok, [{id, 1, "a*"}], 1}, string( "a*" ) ),
   ?_assertEqual( {ok, [{id, 1, "a/"}], 1}, string( "a/" ) )].
  
lisp_should_be_recognized() ->
  ?_assertEqual( {ok, [{lisp, 1, "lisp"}], 1}, string( "lisp" ) ).
  
matlab_should_be_recognized() ->
  ?_assertEqual( {ok, [{matlab, 1, "matlab"}], 1}, string( "matlab" ) ).
  
octave_should_be_recognized() ->
  ?_assertEqual( {ok, [{octave, 1, "octave"}], 1}, string( "octave" ) ).
  
perl_should_be_recognized() ->
  ?_assertEqual( {ok, [{perl, 1, "perl"}], 1}, string( "perl" ) ).
  
python_should_be_recognized() ->
  ?_assertEqual( {ok, [{python, 1, "python"}], 1}, string( "python" ) ).
  
r_should_be_recognized() ->
  [?_assertEqual( {ok, [{r, 1, "r"}], 1}, string( "r" ) ),
   ?_assertEqual( {ok, [{r, 1, "R"}], 1}, string( "R" ) )].

lbrace_should_be_recognized() ->
  ?_assertEqual( {ok, [{lbrace, 1, "{"}], 1}, string( "{" ) ).
  
lparen_should_be_recognized() ->
  ?_assertEqual( {ok, [{lparen, 1, "("}], 1}, string( "(" ) ).
  
lsquarebr_should_be_recognized() ->
  ?_assertEqual( {ok, [{lsquarebr, 1, "["}], 1}, string( "[" ) ).
  
ltag_should_be_recognized() ->
  ?_assertEqual( {ok, [{ltag, 1, "<"}], 1}, string( "<" ) ).
  
nil_should_be_recognized() ->
  ?_assertEqual( {ok, [{nil, 1, "nil"}], 1}, string( "nil" ) ).
  
noreplace_should_be_recognized() ->
  ?_assertEqual( {ok, [{noreplace, 1, "noreplace"}], 1},
                 string( "noreplace" ) ).
  
rbrace_should_be_recognized() ->
  ?_assertEqual( {ok, [{rbrace, 1, "}"}], 1}, string( "}" ) ).
  
rparen_should_be_recognized() ->
  ?_assertEqual( {ok, [{rparen, 1, ")"}], 1}, string( ")" ) ).
  
rsquarebr_should_be_recognized() ->
  ?_assertEqual( {ok, [{rsquarebr, 1, "]"}], 1}, string( "]" ) ).
  
rtag_should_be_recognized() ->
  ?_assertEqual( {ok, [{rtag, 1, ">"}], 1}, string( ">" ) ).
  
semicolon_should_be_recognized() ->
  ?_assertEqual( {ok, [{semicolon, 1, ";"}], 1}, string( ";" ) ).
  
string_should_be_recognized() ->
  ?_assertEqual( {ok, [{string, 1, "String"}], 1}, string( "String" ) ).
  
task_should_be_recognized() ->
  ?_assertEqual( {ok, [{task, 1, "task"}], 1}, string( "task" ) ).
  
then_should_be_recognized() ->
  ?_assertEqual( {ok, [{then, 1, "then"}], 1}, string( "then" ) ).
  
intlit_should_be_recognized() ->
  [?_assertEqual( {ok, [{intlit, 1, "10"}], 1}, string( "10" ) ),
   ?_assertEqual( {ok, [{intlit, 1, "-10"}], 1}, string( "-10" ) ),
   ?_assertEqual( {ok, [{intlit, 1, "9"}], 1}, string( "9" ) ),
   ?_assertEqual( {ok, [{intlit, 1, "0"}], 1}, string( "0" ) )].
   
body_should_be_recognized() ->
  [?_assertEqual( {ok, [{body, 1, ""}], 1}, string( "*{}*" ) ),
   ?_assertEqual( {ok, [{body, 1, "x"}], 1}, string( "*{x}*" ) ),
   ?_assertEqual( {ok, [{body, 1, "xy"}], 1}, string( "*{xy}*" ) ),
   ?_assertEqual( {ok, [{body, 1, "x\ny"}], 2}, string( "*{x\ny}*" ) )].
   
strlit_should_be_recognized() ->
  [?_assertEqual( {ok, [{strlit, 1, ""}], 1}, string( "''" ) ),
   ?_assertEqual( {ok, [{strlit, 1, ""}], 1}, string( "\"\"" ) ),
   ?_assertEqual( {ok, [{strlit, 1, "x"}], 1}, string( "'x'" ) ),
   ?_assertEqual( {ok, [{strlit, 1, "x"}], 1}, string( "\"x\"" ) ),
   ?_assertEqual( {ok, [{strlit, 1, "xy"}], 1}, string( "'xy'" ) ),
   ?_assertEqual( {ok, [{strlit, 1, "xy"}], 1}, string( "\"xy\"" ) )].

   
   
   

-endif.
  

