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

-module( cf ).
-author( "Jörgen Brandt <brandjoe@hu-berlin.de>" ).

-behaviour( application ).

% Application
-export( [start/2, stop/1] ).

% API
-export( [start/0, string/1] ).

%% =============================================================================
%% Application callbacks
%% =============================================================================

start( normal, [] ) ->
  cf_sup:start_link().

stop( _State ) ->
  ok.
  
%% =============================================================================
%% API functions
%% =============================================================================

start() ->
  start( normal, [] ).


string( S ) ->
  {Query, Rho, Gamma} = cf_parser:parse_string( S ),
  reduce( Query, Rho, Gamma ).

reduce( X0, Rho, Gamma ) ->
  X1 = cf_sem:eval( X0, {Rho, fun get_future/1, Gamma, #{}} ),
  case cf_sem:pfinal( X1 ) of
  	true  -> X1;
  	false ->
      receive
        % TODO
        X -> X
      end
  end.

%% =============================================================================
%% Internal Functions
%% =============================================================================

get_future( App ) ->
  gen_server:call( cre, {submit, App} ).


