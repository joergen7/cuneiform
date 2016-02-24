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
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).

% API
-export( [start/0, string/2, file/1, file/2] ).

%% =============================================================================
%% API functions
%% =============================================================================

start() ->
  application:start( cf ).

-spec string( S::string(), DataDir::string() ) -> [cf_sem:str()].

string( S, DataDir ) ->
  {Query, Rho, Gamma} = cf_parse:string( S ),
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
    true  ->
      io:format( "Finished: ~p~nReturning ...~n", [X1] ),
      X1;
    false ->
      io:format( "Not finished: ~p~nOmega: ~p~nWaiting for futures to terminate ...~n", [X1, Omega] ),
      receive

        {failed, script_error, {ActScript, Out}} ->

          % print tool output
          ok = lists:foreach( fun( Line ) ->
                                io:format( "~s~n", [Line] )
                              end,
                              Out ),

          % print actual script
          ok = io:format( "[script]~n" ),
          _ = lists:foldl( fun( Line, N ) ->
                             ok = io:format( "~4.B  ~s~n", [N, Line] ),
                             N+1
                           end,
                           1, string:tokens( ActScript, "\n" ) ),

          error( script_error );

        {failed, Reason, Data} ->
          error( {Reason, Data} );

        {finished, Summary} ->
          Ret = maps:get( ret, Summary ),
          R   = maps:get( prefix, Summary ),
          Delta = lists:foldl(
                    fun( N, Delta0 ) ->
                      acc_delta( N, Delta0, Ret, R )
                    end,
                    #{}, maps:keys( Ret ) ),
          io:format( "Reducing with Omega = ~p~n", [maps:merge( Omega, Delta )] ),
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
  Delta0#{{N, R} => [{str, S} || S <- maps:get( N, Ret )]}.