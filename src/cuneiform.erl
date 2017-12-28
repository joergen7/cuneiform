%% -*- erlang -*-
%%
%% Cuneiform: A Functional Language for Large-Scale Scientific Data Analysis
%%
%% Copyright 2013-2017 Jörgen Brandt
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

%% @author Jörgen Brandt <joergen.brandt@onlinehome.de>

-module( cuneiform ).

-export( [main/1] ).

main( _Args ) ->
  
  % start CRE
  ok = cre:start(),

  % attach workers
  ok = cf_worker:start(),

  % attach client service
  ok = cf_client:start(),

  % start shell
  ok = cuneiform_shell:shell( cf_client ).