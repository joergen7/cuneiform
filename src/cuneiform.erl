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

-module( cuneiform ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).

% API
-export( [main/1, start/0, string/2, file/1, file/2, reduce/4] ).

%% =============================================================================
%% API functions
%% =============================================================================

main( _ ) ->
  start(),
  cf_shell:start().

start() ->
  application:start( cuneiform ).

-spec string( S::string(), DataDir::string() ) -> [cf_sem:str()].

string( S, DataDir ) ->

  % scan
  TokenLst = case cf_scan:string( S ) of
    {error, R1, _}    -> error( R1 );
    {ok, X, _} -> X
  end,

  % parse
  {Query, Rho, Gamma} = case cf_parse:parse( TokenLst ) of
    {error, R2} -> error( R2 );
    {ok, Ret}   -> Ret
  end,

  reduce( Query, Rho, Gamma, DataDir ).

-spec file( Filename::string() ) -> [cf_sem:str()].

file( Filename ) -> file( Filename, "." ).

-spec file( Filename::string(), DataDir::string() ) -> [cf_sem:str()].

file( Filename, DataDir ) ->
  {ok, B} = file:read_file( Filename ),
  S = binary_to_list( B ),
  string( S, DataDir ).



%% =============================================================================
%% Internal Functions
%% =============================================================================

%% Reduction %%

-spec reduce( X0, Rho, Gamma, DataDir ) -> [cf_sem:str()]
when X0      :: [cf_sem:expr()],
     Rho     :: #{string() => [cf_sem:expr()]},
     Gamma   :: #{string() => cf_sem:lam()},
     DataDir :: string().

reduce( X0, Rho, Gamma, DataDir ) ->
  Mu = fun( A ) -> cre:submit( A, DataDir ) end,
  reduce( X0, {Rho, Mu, Gamma, #{}}, DataDir ).

-spec reduce( X0, Theta, DataDir ) -> [cf_sem:str()]
when X0      :: [cf_sem:expr()],
     Theta   :: cf_sem:ctx(),
     DataDir :: string().

reduce( X0, {Rho, Mu, Gamma, Omega}, DataDir ) ->

  X1 = cf_sem:eval( X0, {Rho, Mu, Gamma, Omega} ),
  case cf_sem:pfinal( X1 ) of
    true  -> X1;
    false ->
      receive

        {failed, R2, R, Data} ->
          {AppLine, LamName} = hd( find_select( R, X1 ) ),
          throw( {AppLine, cuneiform, {R2, LamName, R, Data}} );
          

        {finished, Summary} ->
          Ret = maps:get( ret, Summary ),
          R   = maps:get( id, Summary ),
          Delta = lists:foldl(
                    fun( N, Delta0 ) ->
                      acc_delta( N, Delta0, Ret, R )
                    end,
                    #{}, maps:keys( Ret ) ),

          reduce( X1, {Rho, Mu, Gamma, maps:merge( Omega, Delta )}, DataDir );

        Msg -> error( {bad_msg, Msg} )

      end
  end.

-spec acc_delta( N, Delta0, Ret, R ) -> #{string() => [cf_sem:str()]}
when N      :: string(),
     Delta0 :: #{string() => [cf_sem:str()]},
     Ret    :: #{string() => [string()]},
     R      :: pos_integer().

acc_delta( N, Delta0, Ret, R ) ->
  Delta0#{{N, R} => maps:get( N, Ret )}.


find_select( R, L ) when is_list( L ) ->
  lists:flatmap( fun( X ) -> find_select( R, X ) end, L );

find_select( R, Fa ) when is_map( Fa ) ->
  find_select( R, maps:values( Fa ) );

find_select( R, {select, AppLine, _C, {fut, LamName, R, _Lo}} ) ->
  [{AppLine, LamName}];

find_select( R, {app, _AppLine, _C, _Lambda, Fa} ) ->
  find_select( R, Fa );

find_select( _, _ ) ->
  [].

