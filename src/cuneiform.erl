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


-module( cuneiform ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).
-vsn( "2.2.1-snapshot" ).

% API
-export( [main/1, file/2, start/3, reduce/5, get_vsn/0, format_result/1, format_error/1] ).


-include( "cuneiform.hrl" ).

%% =============================================================================
%% API functions
%% =============================================================================

-spec main( CmdLine::list() ) -> ok.

main( CmdLine ) ->
  case getopt:parse( get_optspec_lst(), CmdLine ) of
    {error, {Reason, Data}}   -> error( {Reason, Data} );
    {ok, {OptLst, NonOptLst}} ->
      case lists:member( version, OptLst ) of
        true  -> print_vsn();
        false ->
          case lists:member( help, OptLst ) of
            true  -> print_usage();
            false ->
              case lists:member( cite, OptLst ) of
                true  -> print_bibtex();
                false ->
                  {workdir, Cwd} = lists:keyfind( workdir, 1, OptLst ),
                  {nthread, NSlot} = lists:keyfind( nthread, 1, OptLst ),
                  {platform, Platform} = lists:keyfind( platform, 1, OptLst ),
                  LibMap = get_libmap( OptLst ),
                  {ok, CrePid} = start( Platform, NSlot, LibMap ),
                  link( CrePid ),
                  case NonOptLst of
                    []     -> cf_shell:server( Cwd );
                    [File] ->
                      case file( File, Cwd ) of
                        {ok, Ret}  -> io:format( "~s~n", [format_result( Ret )] );
                        {error, T} -> io:format( "~s~n", [format_error( T )] )
                      end
                  end
              end
          end
      end
  end.

-spec file( File, Cwd ) -> {ok, [cf_sem:str()]} | {error, term()}
when File   :: string(),
     Cwd    :: string().

file( File, Cwd ) ->
  case cf_parse:file( File ) of
    {error, ErrorInfo}        ->
      {error, ErrorInfo};
    {ok, {Query, Rho, Gamma}} ->
      try cuneiform:reduce( cre, Query, Rho, Gamma, Cwd ) of
        X -> {ok, X}
      catch
        throw:T -> {error, T}
      end
  end.


start( Mod, ModArg, LibMap )
when is_atom( Mod ), is_map( LibMap ) ->
  application:start( cuneiform ),
  cf_sup:start_cre( Mod, ModArg, LibMap ).



-spec pprint( X ) -> iolist()
when X::cf_sem:expr() | #{string() => [cf_sem:expr()]}.

pprint( {str, S} )          -> [$", S, $"];
pprint( {var, _Line, S} )   -> S;
pprint( {cnd, Xc, Xt, Xe} ) ->
  ["if ", pprint( Xc ), " then ", pprint( Xt ), " else ", pprint( Xe ), " end"];
pprint( {app, _AppLine, C, {var, _Line, S}, Fa} ) ->
  [S, $(, pprint( Fa ), $)].


%% =============================================================================
%% Internal Functions
%% =============================================================================

get_libmap( OptLst ) ->
  RLib = [P || {rlib, P} <- OptLst],
  PyLib = [P || {pylib, P} <- OptLst],
  #{python => PyLib, r => RLib}.


%% Reduction %%

-spec reduce( Runtime, X0, Rho, Gamma, Cwd ) -> [cf_sem:str()]
when Runtime :: atom() | pid(),
     X0     :: [cf_sem:expr()],
     Rho    :: #{string() => [cf_sem:expr()]},
     Gamma  :: #{string() => cf_sem:lam()},
     Cwd    :: string().

reduce( Runtime, X0, Rho, Gamma, Cwd ) ->
  Mu = fun( A ) -> cf_cre:submit( Runtime, A, Cwd ) end,
  reduce( X0, {Rho, Mu, Gamma, #{}}, Cwd ).

-spec reduce( X0, Theta, Cwd ) -> [cf_sem:str()]
when X0     :: [cf_sem:expr()],
     Theta  :: cf_sem:ctx(),
     Cwd    :: string().

reduce( X0, {Rho, Mu, Gamma, Omega}, Cwd ) ->

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

          reduce( X1, {Rho, Mu, Gamma, maps:merge( Omega, Delta )}, Cwd );

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
  NSlot = case erlang:system_info( logical_processors_available ) of
    unknown -> 1;
    N       -> N
  end,
  [
   {version,  $v,        "version",  undefined,        "show Cuneiform version"},
   {help,     $h,        "help",     undefined,        "show command line options"},
   {cite,     $c,        "cite",     undefined,        "show Bibtex entry for citation"},
   {workdir,  $w,        "workdir",  {string, "."},    "working directory"},
   {nthread,  $t,        "nthread",  {integer, NSlot}, "number of threads in local mode"},
   {platform, $p,        "platform", {atom, local},    "platform to use: local, htcondor"},
   {rlib,     undefined, "rlib",     string,           "include R library path"},
   {pylib,    undefined, "pylib",    string,           "include Python library path"}
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

print_usage() -> getopt:usage( get_optspec_lst(), "cuneiform", "[<scriptfile>]" ).


%% print_vsn/0
%
-spec print_vsn() -> ok.

print_vsn() -> io:format( "~s~n", [get_vsn()] ).

-spec format_result( StrLst::[cf_sem:str()] ) -> iolist().

format_result( StrLst ) ->
  F = fun( {str, S}, AccIn ) ->
        io_lib:format( "~s\"~s\" ", [AccIn, S] )
      end,
  io_lib:format( ?GRN( "~s" ), [lists:foldl( F, "", StrLst )] ).


-spec format_out( [binary()] ) -> iolist().

format_out( Out ) ->
  [io_lib:format( "~s~n", [Line] ) || Line <- Out].



-spec format_script( ActScript::iolist() ) -> iolist().

format_script( ActScript ) ->
  {_, S} = lists:foldl( fun( Line, {N, S} ) ->
                          {N+1, io_lib:format( "~s~4.B  ~s~n", [S, N, Line] )}
                        end,
                        {1, []}, re:split( ActScript, "\n" ) ),
  S.


format_error( {Line, cf_scan, {illegal, Token}} ) ->
  io_lib:format( ?RED( "Line ~p: " )++?BRED( "illegal token ~s" ), [Line, Token] );

format_error( {Line, cf_parse, S} ) ->
  io_lib:format( ?RED( "Line ~p: " )++?BRED( "~s" ), [Line, S] );

format_error( {Line, cf_sem, S} ) ->
  io_lib:format( ?RED( "Line ~p: " )++?BRED( "~s" ), [Line, S] );

format_error( {AppLine, cuneiform, {script_error, LamName, R, {ActScript, Out}}} ) ->
  io_lib:format(
    ?BYLW( "[out]~n" )++?YLW( "~s~n" )
    ++?BYLW( "[script]~n" )++?YLW( "~s~n" )
    ++?RED( "Line ~p: " )++?BRED( "script error in call to ~s (id: ~p)" ),
    [format_out( Out ), format_script( ActScript ), AppLine, LamName, R] );

format_error( {AppLine, cuneiform, {precond, LamName, R, MissingLst}} ) ->
  io_lib:format( ?RED( "Line ~p: " )++?BRED( "precondition not met in call to ~s (~p)~n" )++?RED( "Missing files: ~p" ), [AppLine, LamName, R, MissingLst] );

format_error( ErrorInfo ) ->
  io_lib:format( ?RED( "Error: " )++?BRED( "~p" ), [ErrorInfo] ).
