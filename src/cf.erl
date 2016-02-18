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

-spec string( S::string() ) -> [cf_sem:str()].

string( S ) ->
  {Query, Rho, Gamma} = cf_parser:parse_string( S ),
  reduce( Query, Rho, Gamma ).



%% =============================================================================
%% Internal Functions
%% =============================================================================

%% Reduction %%

-spec reduce( X0, Rho, Gamma ) -> [cf_sem:str()]
when X0    :: [cf_sem:expr()],
     Rho   :: #{string() => [cf_sem:expr()]},
     Gamma :: #{string() => cf_sem:lam()}.

reduce( X0, Rho, Gamma ) ->
  reduce( X0, {Rho, fun cre:submit/1, Gamma, #{}} ).

-spec reduce( X0, Theta ) -> [cf_sem:str()]
when X0    :: [cf_sem:expr()],
     Theta :: cf_sem:ctx().

reduce( X0, {Rho, Mu, Gamma, Omega} ) ->
  X1 = cf_sem:eval( X0, {Rho, fun cre:submit/1, Gamma, #{}} ),
  case cf_sem:pfinal( X1 ) of
    true  -> X1;
    false ->
      receive
        {failed, ActScript, Out} -> error( {failed, ActScript, Out} );
        {finished, Summary} ->
          Ret = maps:get( ret, Summary ),
          Prefix = maps:get( prefix, Summary ),
          Delta = lists:foldl(
                    fun( N, Delta0 ) ->
                      acc_delta( N, Delta0, Ret, Prefix )
                    end,
                    #{}, maps:keys( Ret ) ),
          reduce( X1, {Rho, Mu, Gamma, maps:merge( Omega, Delta )} )
      end
  end.

-spec acc_delta( N, Delta0, Ret, Prefix ) -> #{string() => [cf_sem:str()]}
when N      :: string(),
     Delta0 :: #{string() => [cf_sem:str()]},
     Ret    :: #{string() => [string()]},
     Prefix :: string().

acc_delta( N, Delta0, Ret, Prefix ) ->
  Delta0#{{N, Prefix} => [{str, S} || S <- maps:get( N, Ret )]}.