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

-module(cuneiform_sup).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).

-behaviour( supervisor ).

%% API
-export( [start_link/0] ).

%% Supervisor callbacks
-export([init/1]).


%% =============================================================================
%% API functions
%% =============================================================================

start_link() ->
  supervisor:start_link( {local, ?MODULE}, ?MODULE, [] ).

%% =============================================================================
%% Supervisor callbacks
%% =============================================================================

init( [] ) ->

  RestartStrategy = one_for_one,
  MaxRestarts = 5,
  MaxSecondsBetweenRestarts = 10,
  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = temporary,
  Shutdown = 2000,
  Type = worker,
  Cre = {cre, {cre, start_link, []}, Restart, Shutdown, Type, [cre]},

  {ok, {SupFlags, [Cre]}}.

