%% -*- erlang -*-
%%
%% A functional workflow language for large-scale scientific data analysis.
%%
%% Copyright 2015-2018 Jörgen Brandt
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @author Jörgen Brandt <joergen.brandt@onlinehome.de>
%% @version 3.0.0
%% @copyright 2015-2018 Jörgen Brandt
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

%% ==================================================================
%% Definitions
%% ==================================================================


Definitions.

LBASH     = Bash
LOCTAVE   = Octave
LPERL     = Perl
LPYTHON   = Python
LR        = R
LRACKET   = Racket

TSTR      = Str
TFILE     = File
TBOOL     = Bool
TFNNTV    = Ntv
TFNFRN    = Frn

ASSIGN    = let
BAR       = \|
CMP       = ==
CND       = if
COLON     = :
COMMA     = ,
DEF       = def
DO        = do
DOT       = \.
ELSE      = else
EQ        = =
FALSE     = false
FOLD      = fold
FOR       = for
HALT      = end
IMPORT    = import
IN        = in
ISNIL     = isnil
LARROW    = <-
LBRACE    = \{
LPAREN    = \(
LSQUAREBR = \[
LTAG      = <
NEG       = not
PLUS      = \+
RARROW    = ->
RBRACE    = \}
RPAREN    = \)
RSQUAREBR = \]
RTAG      = >
SEMICOLON = ;
THEN      = then
TRUE      = true
VEE       = or
WEDGE     = and

ID        = [A-Za-z][A-Za-z0-9\.\-_]*
INTLIT    = -?([0-9]|[1-9][0-9]*)
STRLIT    = "[^"]*"
FILELIT   = '[^']*'
BODY      = \*\{([^}]|\}+[^}*])*\}+\*

COMMENT   = %.*
WS        = [\000-\s]


%% ==================================================================
%% Rules
%% ==================================================================

Rules.

{LBASH}     : {token, {l_bash, TokenLine, TokenChars}}.
{LOCTAVE}   : {token, {l_octave, TokenLine, TokenChars}}.
{LPERL}     : {token, {l_perl, TokenLine, TokenChars}}.
{LPYTHON}   : {token, {l_python, TokenLine, TokenChars}}.
{LR}        : {token, {l_r, TokenLine, TokenChars}}.
{LRACKET}   : {token, {l_racket, TokenLine, TokenChars}}.

{TSTR}      : {token, {t_str, TokenLine, TokenChars}}.
{TFILE}     : {token, {t_file, TokenLine, TokenChars}}.
{TBOOL}     : {token, {t_bool, TokenLine, TokenChars}}.
{TFNNTV}    : {token, {t_fn_ntv, TokenLine, TokenChars}}.
{TFNFRN}    : {token, {t_fn_frn, TokenLine, TokenChars}}.

{ASSIGN}    : {token, {assign, TokenLine, TokenChars}}.
{BAR}       : {token, {bar, TokenLine, TokenChars}}.
{CMP}       : {token, {cmp, TokenLine, TokenChars}}.
{CND}       : {token, {cnd, TokenLine, TokenChars}}.
{COLON}     : {token, {colon, TokenLine, TokenChars}}.
{COMMA}     : {token, {comma, TokenLine, TokenChars}}.
{DEF}       : {token, {def, TokenLine, TokenChars}}.
{DO}        : {token, {do, TokenLine, TokenChars}}.
{DOT}       : {token, {dot, TokenLine, TokenChars}}.
{ELSE}      : {token, {else, TokenLine, TokenChars}}.
{HALT}      : {token, {halt, TokenLine, TokenChars}}.
{EQ}        : {token, {eq, TokenLine, TokenChars}}.
{FALSE}     : {token, {false, TokenLine, TokenChars}}.
{FOLD}      : {token, {fold, TokenLine, TokenChars}}.
{FOR}       : {token, {for, TokenLine, TokenChars}}.
{IMPORT}    : {token, {import, TokenLine, TokenChars}}.
{IN}        : {token, {in, TokenLine, TokenChars}}.
{ISNIL}     : {token, {isnil, TokenLine, TokenChars}}.
{LARROW}    : {token, {larrow, TokenLine, TokenChars}}.
{LBRACE}    : {token, {lbrace, TokenLine, TokenChars}}.
{LPAREN}    : {token, {lparen, TokenLine, TokenChars}}.
{LSQUAREBR} : {token, {lsquarebr, TokenLine, TokenChars}}.
{LTAG}      : {token, {ltag, TokenLine, TokenChars}}.
{NEG}       : {token, {neg, TokenLine, TokenChars}}.
{PLUS}      : {token, {plus, TokenLine, TokenChars}}.
{RARROW}    : {token, {rarrow, TokenLine, TokenChars}}.
{RBRACE}    : {token, {rbrace, TokenLine, TokenChars}}.
{RPAREN}    : {token, {rparen, TokenLine, TokenChars}}.
{RSQUAREBR} : {token, {rsquarebr, TokenLine, TokenChars}}.
{RTAG}      : {token, {rtag, TokenLine, TokenChars}}.
{SEMICOLON} : {token, {semicolon, TokenLine, TokenChars}}.
{THEN}      : {token, {then, TokenLine, TokenChars}}.
{TRUE}      : {token, {true, TokenLine, TokenChars}}.
{VEE}       : {token, {vee, TokenLine, TokenChars}}.
{WEDGE}     : {token, {wedge, TokenLine, TokenChars}}.

{ID}        : {token, {id, TokenLine, TokenChars}}.
{INTLIT}    : {token, {intlit, TokenLine, TokenChars}}.
{STRLIT}    : {token, {strlit, TokenLine, trim_lit( TokenChars )}}.
{FILELIT}   : {token, {filelit, TokenLine, trim_lit( TokenChars )}}.
{BODY}      : {token, {body, TokenLine, trim_body( TokenChars )}}.

{COMMENT}   : skip_token.
{WS}        : skip_token.


%% ==================================================================
%% Erlang Code
%% ==================================================================


Erlang code.

-export( [yyrev/2] ).

-spec trim_body( S :: string() ) -> string().

trim_body( S )
when is_list( S ), length( S ) >= 4 ->
  string:substr( S, 3, length( S )-4 ).


-spec trim_lit( S :: string() ) -> string().

trim_lit( S )
when is_list( S ), length( S ) >= 2 ->
  string:substr( S, 2, length( S )-2 ).