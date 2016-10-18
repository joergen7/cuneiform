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

-export( [create_conf/3] ).

-spec create_conf( DefaultMap, ConfFile, ManualMap ) -> #{ _ => _ }
when DefaultMap :: #{ _ => _},
     ConfFile   :: string(),
     ManualMap  :: #{ _ => _ }.

create_conf( DefaultMap, ConfFile, ManualMap )
when is_map( DefaultMap ),
     is_list( ConfFile ),
     is_map( ManualMap ) ->

  Merge = case file:read_file( ConfFile ) of
      {error, enoent}  -> DefaultMap;
      {error, Reason1} -> error( {Reason1, ConfFile} );
      {ok, B}          ->
        S = binary_to_list( B ),
        {ok, Tokens, _} = erl_scan:string( S ),
        ConfMap = case erl_parse:parse_term( Tokens ) of
          {error, Reason2} -> error( Reason2 );
          {ok, Y}         -> Y
        end,
        maps:map( fun( K, V ) -> maps:get( K, ConfMap, V ) end, DefaultMap )
    end,

  maps:map( fun( K, V ) -> maps:get( K, ManualMap, V ) end, Merge ).
