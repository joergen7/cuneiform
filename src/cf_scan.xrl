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
%% Definitions
%% =============================================================================


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




%% =============================================================================
%% Rules
%% =============================================================================

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




%% =============================================================================
%% Erlang Code
%% =============================================================================


Erlang code.

-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).

-export( [yyrev/2] ).

-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).
-endif.

trim_body( S ) ->
  string:substr( S, 3, length( S )-4 ).
  
trim_strlit( S ) ->
  string:substr( S, 2, length( S )-2 ).
  


%% =============================================================================
%% Unit Tests
%% =============================================================================

  
-ifdef( TEST ).



bash_style_comment_should_be_recognized_test() ->
  ?assertEqual( {ok, [], 1}, string( "#this is a comment" ) ).
  
c_style_comment_should_be_recognized_test() ->
  ?assertEqual( {ok, [], 1}, string( "//this is a comment" ) ).
  
erlang_style_comment_should_be_recognized_test() ->
  ?assertEqual( {ok, [], 1}, string( "%this is a comment" ) ).
  
  
multiline_comment_should_be_recognized_test() ->
  [?assertEqual( {ok, [], 1}, string( "/**/" ) ),
   ?assertEqual( {ok, [], 1}, string( "/*x*/" ) ),
   ?assertEqual( {ok, [], 1}, string( "/*xy*/" ) ),
   ?assertEqual( {ok, [], 2}, string( "/*x\ny*/" ) )].
    
bash_should_be_recognized_test() ->
  ?assertEqual( {ok, [{bash, 1, "bash"}], 1}, string( "bash" ) ).
  
beginif_should_be_recognized_test() ->
  ?assertEqual( {ok, [{beginif, 1, "if"}], 1}, string( "if" ) ).
  
colon_should_be_recognized_test() ->
  ?assertEqual( {ok, [{colon, 1, ":"}], 1}, string( ":" ) ).
  
deftask_should_be_recognized_test() ->
  ?assertEqual( {ok, [{deftask, 1, "deftask"}], 1}, string( "deftask" ) ).

else_should_be_recognized_test() ->
  ?assertEqual( {ok, [{else, 1, "else"}], 1}, string( "else" ) ).

endif_should_be_recognized_test() ->
  ?assertEqual( {ok, [{endif, 1, "end"}], 1}, string( "end" ) ).

eq_should_be_recognized_test() ->
  ?assertEqual( {ok, [{eq, 1, "="}], 1}, string( "=" ) ).

file_should_be_recognized_test() ->
  ?assertEqual( {ok, [{file, 1, "File"}], 1}, string( "File" ) ).

in_should_be_recognized_test() ->
  ?assertEqual( {ok, [{in, 1, "in"}], 1}, string( "in" ) ).
  
id_should_be_recognized_test() ->
  [?assertEqual( {ok, [{id, 1, "inp"}], 1}, string( "inp" ) ),
   ?assertEqual( {ok, [{id, 1, "a0"}], 1}, string( "a0" ) ),
   ?assertEqual( {ok, [{id, 1, "a9"}], 1}, string( "a9" ) ),
   ?assertEqual( {ok, [{id, 1, "a."}], 1}, string( "a." ) ),
   ?assertEqual( {ok, [{id, 1, "a-"}], 1}, string( "a-" ) ),
   ?assertEqual( {ok, [{id, 1, "a_"}], 1}, string( "a_" ) )].
    
python_should_be_recognized_test() ->
  ?assertEqual( {ok, [{python, 1, "python"}], 1}, string( "python" ) ).
  
r_should_be_recognized_test() ->
  [?assertEqual( {ok, [{r, 1, "r"}], 1}, string( "r" ) ),
   ?assertEqual( {ok, [{r, 1, "R"}], 1}, string( "R" ) )].

lbrace_should_be_recognized_test() ->
  ?assertEqual( {ok, [{lbrace, 1, "{"}], 1}, string( "{" ) ).
  
lparen_should_be_recognized_test() ->
  ?assertEqual( {ok, [{lparen, 1, "("}], 1}, string( "(" ) ).
  
lsquarebr_should_be_recognized_test() ->
  ?assertEqual( {ok, [{lsquarebr, 1, "["}], 1}, string( "[" ) ).
  
ltag_should_be_recognized_test() ->
  ?assertEqual( {ok, [{ltag, 1, "<"}], 1}, string( "<" ) ).
  
nil_should_be_recognized_test() ->
  ?assertEqual( {ok, [{nil, 1, "nil"}], 1}, string( "nil" ) ).
    
rbrace_should_be_recognized_test() ->
  ?assertEqual( {ok, [{rbrace, 1, "}"}], 1}, string( "}" ) ).
  
rparen_should_be_recognized_test() ->
  ?assertEqual( {ok, [{rparen, 1, ")"}], 1}, string( ")" ) ).
  
rsquarebr_should_be_recognized_test() ->
  ?assertEqual( {ok, [{rsquarebr, 1, "]"}], 1}, string( "]" ) ).
  
rtag_should_be_recognized_test() ->
  ?assertEqual( {ok, [{rtag, 1, ">"}], 1}, string( ">" ) ).
  
semicolon_should_be_recognized_test() ->
  ?assertEqual( {ok, [{semicolon, 1, ";"}], 1}, string( ";" ) ).
  
string_should_be_recognized_test() ->
  ?assertEqual( {ok, [{string, 1, "String"}], 1}, string( "String" ) ).
  
then_should_be_recognized_test() ->
  ?assertEqual( {ok, [{then, 1, "then"}], 1}, string( "then" ) ).
  
intlit_should_be_recognized_test() ->
  [?assertEqual( {ok, [{intlit, 1, "10"}], 1}, string( "10" ) ),
   ?assertEqual( {ok, [{intlit, 1, "-10"}], 1}, string( "-10" ) ),
   ?assertEqual( {ok, [{intlit, 1, "9"}], 1}, string( "9" ) ),
   ?assertEqual( {ok, [{intlit, 1, "0"}], 1}, string( "0" ) )].
   
body_should_be_recognized_test() ->
  [?assertEqual( {ok, [{body, 1, ""}], 1}, string( "*{}*" ) ),
   ?assertEqual( {ok, [{body, 1, "x"}], 1}, string( "*{x}*" ) ),
   ?assertEqual( {ok, [{body, 1, "xy"}], 1}, string( "*{xy}*" ) ),
   ?assertEqual( {ok, [{body, 1, "x\ny"}], 2}, string( "*{x\ny}*" ) )].
   
strlit_should_be_recognized_test() ->
  [?assertEqual( {ok, [{strlit, 1, ""}], 1}, string( "\"\"" ) ),
   ?assertEqual( {ok, [{strlit, 1, "x"}], 1}, string( "\"x\"" ) ),
   ?assertEqual( {ok, [{strlit, 1, "xy"}], 1}, string( "\"xy\"" ) )].

   
   
   

-endif.
  

