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

-module( cf_shell ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).
-vsn( "2.2.0" ).

%% =============================================================================
%% Function Exports
%% =============================================================================

-export( [start/0] ).

%% =============================================================================
%% Type Definitions
%% =============================================================================


%% =============================================================================
%% Macro Definitions
%% =============================================================================

-define( BUILD, "2016-03-22" ).

-define( RED( Str ), "\e[31m" ++ Str ++ "\e[0m" ).
-define( BRED( Str ), "\e[1;31m" ++ Str ++ "\e[0m" ).
-define( GRN( Str ), "\e[32m" ++ Str ++ "\e[0m" ).
-define( YLW( Str ), "\e[33m" ++ Str ++ "\e[0m" ).
-define( BYLW( Str ), "\e[1;33m" ++ Str ++ "\e[0m" ).
-define( BLU( Str ), "\e[1;34m" ++ Str ++ "\e[0m" ).

-define( PROMPT, "> " ).

%% =============================================================================
%% Internal Functions
%% =============================================================================


start() ->
  process_flag( trap_exit, true ),
  io:format( "~s~n~n~n", [get_banner()] ),
  server_loop( #{}, #{}, "." ).

server_loop( Rho, Gamma, Cwd ) ->
  case read_expression( ?PROMPT ) of
    {ctl, quit}                 ->
      ok;
    {ctl, ls}                   ->
      {ok, FileLst} = file:list_dir( Cwd ),
      io:format( "~p~n", [FileLst] ),
      server_loop( Rho, Gamma, Cwd );
    {ctl, cwd}                  ->
      io:format( "~s~n", [filename:absname( Cwd )] ),
      server_loop( Rho, Gamma, Cwd );
    {ctl, help}                 ->
      io:format( "~s~n", [get_help()] ),
      server_loop( Rho, Gamma, Cwd );
    {ctl, state}                ->
      io:format( "~p~n", [Rho] ),
      server_loop( Rho, Gamma, Cwd );
    {ctl, tasks}                ->
      io:format( "~p~n", [Gamma] ),
      server_loop( Rho, Gamma, Cwd );
    {error, ErrorInfo}          ->
      S = format_error( ErrorInfo ),
      io:format( "~s~n", [S] ),
      server_loop( Rho, Gamma, Cwd );
    {ok, {Query, DRho, DGamma}} ->
      Rho1 = maps:merge( Rho, DRho ),
      Gamma1 = maps:merge( Gamma, DGamma ),
      case Query of
        undef -> server_loop( Rho1, Gamma1, Cwd );
        _     ->
          try cuneiform:reduce( Query, Rho, Gamma, "." ) of
            X -> io:format( "~s~n", [format_result( X )] )
          catch
            throw:T -> io:format( "~s~n", [format_error( T )] )
          end,
          server_loop( Rho, Gamma, Cwd )
      end
  end.

read_expression( Prompt ) ->
  io:format( Prompt ),
  read( [] ).

read( Buf ) ->
  case io:get_line( "" ) of
    eof -> {ctl, quit};
    S   ->
      T = Buf++S,
      {ok, TokenLst, _} = cf_prescan:string( T ),
      case TokenLst of
        [] ->
          case Buf of
            []    -> {ok, {undef, #{}, #{}}};      % nothing entered or buffered
            [_|_] -> read( Buf )                   % nothing new entered
          end;
        [_|_] ->                                   % something was entered
          case is_open( TokenLst ) of
            true  -> read( T );                    % closing paren missing
            false ->
              case lists:last( TokenLst ) of
                semicolon -> cf_parse:string( T );
                rbrace    -> cf_parse:string( T );
                rmmecb    -> cf_parse:string( T );

                lbrace    -> cf_parse:string( T ); % these are in fact error
                lmmecb    -> cf_parse:string( T ); % cases but cf_parse will
                                                   % figure it out

                nonws     -> read( T );
                C         -> {ctl, C}
              end
          end
      end
  end.

is_open( TokenLst ) ->
  NOpen1 = length( [lbrace || lbrace <- TokenLst] ),
  NClose1 = length( [rbrace || rbrace <- TokenLst] ),
  NOpen2 = length( [lmmecb || lmmecb <- TokenLst] ),
  NClose2 = length( [rmmecb || rmmecb <- TokenLst] ),
  NOpen1 > NClose1 orelse NOpen2 > NClose2.

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
    ++?RED( "Line ~p: " )++?BRED( "script error in call to ~s (~p)" ),
    [format_out( Out ), format_script( ActScript ), AppLine, LamName, R] );

format_error( {AppLine, cuneiform, {precond, LamName, R, MissingLst}} ) ->
  io_lib:format( ?RED( "Line ~p: " )++?BRED( "precondition not met in call to ~s (~p)~n" )++?RED( "Missing files: ~p" ), [AppLine, LamName, R, MissingLst] );

format_error( ErrorInfo ) ->
  io_lib:format( ?RED( "Error: " )++?BRED( "~p" ), [ErrorInfo] ).

format_result( StrLst ) ->
  F = fun( {str, S}, AccIn ) ->
        io_lib:format( "~s\"~s\" ", [AccIn, S] )
      end,
  io_lib:format( ?GRN( "~s" ), [lists:foldl( F, "", StrLst )] ).


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

-spec get_help() -> iolist().

get_help() ->
  string:join(
    [?YLW( "help" )++"  show this usage info",
     ?YLW( "state" )++" show variable bindings",
     ?YLW( "tasks" )++" show task definitions",
     ?YLW( "ls" )++"    list files",
     ?YLW( "cwd" )++"   current working directory",
     ?YLW( "quit" )++"  quit the shell"
    ], "\n" ).




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

print_usage() -> getopt:usage( get_optspec_lst(), "cuneiform", "<scriptfile>" ).


%% opt_spec_list/0
%
-spec get_optspec_lst() -> [{atom(), char(), string(), undefined, string()}].

get_optspec_lst() ->
  [
   {version,  $v, "version",  undefined,         "Show Cuneiform version"},
   {help,     $h, "help",     undefined,         "Show command line options"},
   {cite,     $c, "cite",     undefined,         "Show Bibtex entry for citation"}
  ].