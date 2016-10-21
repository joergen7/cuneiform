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


%% @doc The cuneiform supervisor (sup) starts (and restarts if necessary) the
%% child process that implements a specific platform - the Cuneiform Runtime
%% Environment (CRE). 
%% It also starts (and restarts if necessary) the log manager (logmgr).
%% It is called from {@link cuneiform:start/1}.
%% When started (by starting the {@link cuneiform_app}), only the log manager
%% is started, a specific CRE implementation is started through {@link start_cre/3}.
%% 
%% ```
%%            +-----+
%%            | sup |
%%            +-----+
%%           /       \
%%    +--------+      +-------+
%%    | logmgr |      |  CRE  |
%%    +--------+      +-------+'''

%% @author Jörgen Brandt <brandjoe@hu-berlin.de>


-module( cf_sup ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).

-behaviour( supervisor ).

%% API
-export( [start_link/0, start_cre/3] ).

%% Supervisor callbacks
-export([init/1]).


%% =============================================================================
%% API functions
%% =============================================================================

%% start_link/0
%% starts a supervisor process using the behavior implemented in this module 
start_link() ->
  supervisor:start_link( {local, ?MODULE}, ?MODULE, [] ).

%% start_cre/3
%% @doc adds the runtime environment process as a child to the supervision tree.
%% This makes the runtime environment a sibling of the log manager, also supervised
%% by the cuneiform supervisor (and started in the {@link init/1} callback implementation)
%% The actual runtime startup process is implemented by start_link, (e.g. in {@link local} or {@link htcondor}).
%% Since the runtimes implement cf_cre, they indirectly implement the gen_server behavior.
-spec start_cre( Mod, ModArg, LibMap ) -> {ok, pid()}
when Mod    :: atom(),
     ModArg :: term(),
     LibMap :: #{cf_sem:lang() => [string()]}.

start_cre( Mod, ModArg, LibMap )
when is_atom( Mod ), is_map( LibMap ) ->
  
  RuntimeEnvChildSpec = #{
    id => cre, 
    start => {cf_cre, start_link, [Mod, ModArg, LibMap]},
    restart => temporary,   % never restart
    modules => [cf_cre]
  },

  supervisor:start_child( ?MODULE, RuntimeEnvChildSpec ).

%% =============================================================================
%% Supervisor callbacks
%% =============================================================================

%% init/1
%% @doc returns the supervisor configuration (flags) and the definition and 
%% configuration of the child processes, i.e., the log manager.
init( [] ) ->

  SupFlags = #{
    strategy => one_for_one, 
    intensity => 3, % if more than <intensity> restarts occur...
    period => 10    % ...within <period> seconds, terminate child processes and self
  },

  Logmgr = #{
    id       => logmgr,
    start    => {logmgr, start_link, []}, %start    => {gen_event, start_link, [{local, logmgr}]},
    restart  => permanent,
    shutdown => 10000,   % timeout [ms] to wait for an exit signal with reason shutdown from the child process.
    type     => worker,
    modules  => dynamic  % If the child process is an event manager (gen_event) with a dynamic set of callback modules, value dynamic must be used. [erlang docs]
  },

  {ok, {SupFlags, [Logmgr]}}.
