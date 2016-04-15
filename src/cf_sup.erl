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


-module( cf_sup ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).
-vsn( "2.2.1-snapshot" ).


-behaviour( supervisor ).

%% API
-export( [start_link/0, start_cre/3] ).

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


  {ok, {SupFlags, []}}.

-spec start_cre( Mod, ModArg, LibMap ) -> {ok, pid()}
when Mod    :: atom(),
     ModArg :: term(),
     LibMap :: #{cf_sem:lang() => [string()]}.

start_cre( Mod, ModArg, LibMap )
when is_atom( Mod ), is_map( LibMap ) ->

  Restart = temporary,
  Shutdown = 2000,
  Type = worker,

  Cre = {cre, {cf_cre, start_link, [Mod, ModArg, LibMap]}, Restart, Shutdown, Type, [cf_cre]},

  supervisor:start_child( cf_sup, Cre ).