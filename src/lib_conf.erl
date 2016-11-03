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

-module( lib_conf ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).

-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).
-endif.

-export( [create_conf/3] ).

%% create_conf/3
%% @doc Creates a configuration map starting from a default configuration.
%% If a configuration file (ConfFile) is found, its values override the default
%% configuration. However, keys that are not part of the DefaultMap are discarded.
%% In the same manner, the values of the ManualMap are incorporated into the result. 
%% The override order is ManualMap > ConfFile > DefaultMap
-spec create_conf( DefaultMap, ConfFile, ManualMap ) -> #{ _ => _ }
when DefaultMap :: #{ _ => _},
     ConfFile   :: string(),
     ManualMap  :: #{ _ => _ }.

create_conf( DefaultMap, ConfFile, ManualMap )
when is_map( DefaultMap ),
     is_list( ConfFile ),
     is_map( ManualMap ) ->

  % merge default map and configuration file
  Merge = case file:read_file( ConfFile ) of
      % configuration file is allowed to miss, use default map
      {error, enoent}  -> DefaultMap;
      {error, Reason1} -> error( {Reason1, ConfFile} );
      % if found, load and override settings DefaultMap 
      {ok, B}          ->
        S = binary_to_list( B ),
        {ok, Tokens, _} = erl_scan:string( S ),
        ConfMap = case erl_parse:parse_term( Tokens ) of
          {error, Reason2} -> error( Reason2 );
          {ok, Y}         -> Y
        end,
        % for each key in the default map, look up the value in the ConfMap and use it instead
        % the lookup of key nthread in ConfMap doesn't cause an error, since V is default
        maps:map( fun( K, V ) -> maps:get( K, ConfMap, V ) end, DefaultMap )
    end,
  % for each key in the Merge map, look up the value in the ManualMap and use it instead
  maps:map( fun( K, V ) -> maps:get( K, ManualMap, V ) end, Merge ).

%% =============================================================================
%% Unit Tests
%% =============================================================================

-ifdef( TEST ).

simple_create_conf_test_() ->

  DefaultMap = #{ nthread => 1, profiling => false },
  % keys in the manual map that do not appear in the default map are discarded
  ManualMap = #{ nthread => 4, some_nonsense => whacko },
  Expected = #{ profiling => false, nthread => 4 },
  
  Conf = create_conf( DefaultMap, "", ManualMap),

  ?_assert( Conf == Expected ).

-endif.