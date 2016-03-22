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
-vsn( "2.2.0" ).

% API
-export( [main/1, start/0, string/2, file/1, file/2, reduce/4, get_banner/0] ).


-include( "cuneiform.hrl" ).

%% =============================================================================
%% API functions
%% =============================================================================

main( CmdLine ) ->

  case getopt:parse( get_optspec_lst(), CmdLine ) of
    {error, {Reason, Data}}   -> error( {Reason, Data} );
    {ok, {OptLst, _NonOptLst}} ->
      case lists:member( version, OptLst ) of
        true  -> print_vsn();
        false ->
          case lists:member( help, OptLst ) of
            true ->
              print_usage();
            false ->
              case lists:member( cite, OptLst ) of
                true -> print_bibtex();
                false ->
                  {workdir, WorkDir} = lists:keyfind( workdir, 1, OptLst ),
                  start(),
                  cf_shell:server( WorkDir )
              end
          end
      end
  end.

start() ->
  application:start( cuneiform ).

-spec string( S::string(), DataDir::string() ) -> [cf_sem:str()].

string( S, DataDir ) ->
  {ok, {Query, Rho, Gamma}} = cf_parse:string( S ),
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



%% opt_spec_list/0
%
-spec get_optspec_lst() -> [{atom(), char(), string(), undefined, string()}].

get_optspec_lst() ->
  [
   {version,  $v, "version",  undefined,         "Show Cuneiform version"},
   {help,     $h, "help",     undefined,         "Show command line options"},
   {cite,     $c, "cite",     undefined,         "Show Bibtex entry for citation"},
   {workdir,  $w, "workdir",  {string, "."},     "Working directory"}
  ].

-spec get_vsn() -> string().

get_vsn() ->
  {vsn, Vsn} = lists:keyfind( vsn, 1, module_info( attributes ) ),
  Vsn.


%% print_bibtex/0
%
-spec print_bibtex() -> ok.

print_bibtex() -> io:format( "~n~s~n~n", [get_bibtex()] ).


%% get_bibtex/0
%
-spec get_bibtex() -> iolist().

get_bibtex() ->
  string:join( ["@InProceedings{Brandt2015,",
         "  Title                    = {Cuneiform: A Functional Language for Large Scale Scientific Data Analysis},",
         "  Author                   = {Brandt, J{\"o}rgen and Bux, Marc and Leser, Ulf},",
         "  Booktitle                = {Proceedings of the Workshops of the EDBT/ICDT},",
         "  Year                     = {2015},",
         "  Address                  = {Brussels, Belgium},",
         "  Month                    = {March},",
         "  Pages                    = {17--26},",
         "  Volume                   = {1330},",
         "  Abstract                 = {The need to analyze massive scientific data sets on the one hand and the availability of distributed compute resources with an increasing number of CPU cores on the other hand have promoted the development of a variety of languages and systems for parallel, distributed data analysis. Among them are data-parallel query languages such as Pig Latin or Spark as well as scientific workflow languages such as Swift or Pegasus DAX. While data-parallel query languages focus on the exploitation of data parallelism, scientific workflow languages focus on the integration of external tools and libraries. However, a language that combines easy integration of arbitrary tools, treated as black boxes, with the ability to fully exploit data parallelism does not exist yet. Here, we present Cuneiform, a novel language for large-scale scientific data analysis. We highlight its functionality with respect to a set of desirable features for such languages, introduce its syntax and semantics by example, and show its flexibility and conciseness with use cases, including a complex real-life workflow from the area of genome research. Cuneiform scripts are executed dynamically on the workflow execution platform Hi-WAY which is based on Hadoop YARN. The language Cuneiform, including tool support for programming, workflow visualization, debugging, logging, and provenance-tracing, and the parallel execution engine Hi-WAY are fully implemented.},",
         "  Doi                      = {10.13140/RG.2.1.3547.6561},",
         "  Url                      = {http://ceur-ws.org/Vol-1330/paper-03.pdf}",
         "}"], "\n" ).

%% print_usage/0
%
-spec print_usage() -> ok.

print_usage() -> getopt:usage( get_optspec_lst(), "cuneiform", "[scriptfile]" ).


%% print_vsn/0
%
-spec print_vsn() -> ok.

print_vsn() -> io:format( "~s~n", [get_vsn()] ).

-spec get_banner() -> iolist().

get_banner() ->
  string:join(
    ["            ___",
     "           @@WB      Cuneiform",
     "          @@E_____   "++get_vsn()++" "++?BUILD,
     "     _g@@@@@WWWWWWL",
     "   g@@#*`3@B         "++?YLW( "Type " )++?BYLW( "help" )++?YLW( " for usage info." ),
     "  @@P    3@B",
     "  @N____ 3@B         Docs: "++?BLU( "http://www.cuneiform-lang.org" ),
     "  \"W@@@WF3@B         Code: "++?BLU( "https://github.com/joergen7/cuneiform" )
    ], "\n" ).
