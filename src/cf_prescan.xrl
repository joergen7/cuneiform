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

LS        = ls
CWD       = cwd
HELP      = help
NONWS     = .
QUIT      = quit
SEMICOLON = ;
LBRACE    = \{
LMMECB    = \*\{
RBRACE    = \}
RMMECB    = \}\*
STATE     = state
TASKS     = tasks
WS        = [\000-\s]


%% =============================================================================
%% Rules
%% =============================================================================

Rules.

{LS}        : {token, ls}.
{CWD}       : {token, cwd}.
{TASKS}     : {token, tasks}.
{STATE}     : {token, state}.
{QUIT}      : {token, quit}.
{HELP}      : {token, help}.
{SEMICOLON} : {token, semicolon}.
{LBRACE}    : {token, lbrace}.
{RBRACE}    : {token, rbrace}.
{LMMECB}    : {token, lmmecb}.
{RMMECB}    : {token, rmmecb}.
{WS}        : skip_token.
{NONWS}     : {token, nonws}.

%% =============================================================================
%% Erlang Code
%% =============================================================================


Erlang code.

-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).

-export( [yyrev/2] ).