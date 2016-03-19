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

-define( VSN, "2.2.0" ).
-define( BUILD, "2016-03-17" ).

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
  server_loop( #{}, #{} ).

server_loop( Rho, Gamma ) ->
  case read_expression( ?PROMPT ) of
    {ctl, quit}                 ->
      ok;
    {ctl, help}                 ->
      io:format( "~n~s~n~n", [get_help()] ),
      server_loop( Rho, Gamma );
    {ctl, state}                ->
      io:format( "~p~n", [Rho] ),
      server_loop( Rho, Gamma );
    {ctl, tasks}                ->
      io:format( "~p~n", [Gamma] ),
      server_loop( Rho, Gamma );
    {error, ErrorInfo}          ->
      S = format_error( ErrorInfo ),
      io:format( "~s~n", [S] ),
      server_loop( Rho, Gamma );
    {ok, {Query, DRho, DGamma}} ->
      Rho1 = maps:merge( Rho, DRho ),
      Gamma1 = maps:merge( Gamma, DGamma ),
      case Query of
        undef -> server_loop( Rho1, Gamma1 );
        _     ->
          try cuneiform:reduce( Query, Rho, Gamma, "." ) of
            X -> io:format( "~s~n", [format_result( X )] )
          catch
            throw:T -> io:format( "~s~n", [format_error( T )] )
          end,
          server_loop( Rho, Gamma )
      end
  end.



read_expression( Prompt ) ->
  Read = fun() ->
           io:format( Prompt ),
           Ret = read( [] ),
           exit( Ret )
         end,
  Rdr = spawn_link( Read ),
  receive
    {'EXIT', Rdr, Ret} -> Ret;
    Msg                -> error( {bad_msg, Msg} )
  end.

read( Buf ) ->
  S = io:get_line( "" ),
  {ok, TokenLst, _} = pre_scan:string( S ),
  case TokenLst of
    [] ->
      case Buf of
        []    -> {ok, {undef, #{}, #{}}};
        [_|_] -> read( Buf )
      end;
    [_|_] ->
      case lists:last( TokenLst ) of
        terminal -> parse_string( Buf++S );
        nonws    -> read( Buf++S );
        C        -> {ctl, C}
      end
  end.

parse_string( S ) ->
  case cf_scan:string( S ) of
    {error, ScanErrorInfo, _} -> {error, ScanErrorInfo};
    {ok, TokenLst, _}         ->
      case cf_parse:parse( TokenLst ) of
        {error, ParseErrorInfo} -> {error, ParseErrorInfo};
        {ok, Triple}            -> {ok, Triple}
      end
  end.


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


get_banner() ->
  string:join(
    ["            ___",
     "           @@WB      Cuneiform",
     "          @@E_____   "++?VSN++" "++?BUILD,
     "     _g@@@@@WWWWWWL",
     "   g@@#*`3@B         Type "++?BYLW( "help" )++" for usage info.",
     "  @@P    3@B         Type "++?BYLW( "quit" )++" to quit.",
     "  @N____ 3@B         Docs: "++?BLU( "http://www.cuneiform-lang.org" ),
     "  \"W@@@WF3@B         Code: "++?BLU( "https://github.com/joergen7/cuneiform" )
    ], "\n" ).

get_help() ->
  string:join(
    ["help  -- Show this usage info",
     "quit  -- Quit the shell",
     "state -- Show variable bindings",
     "tasks -- Show task definitions"
    ], "\n" ).






format_out( Out ) ->
  [io_lib:format( "~s~n", [Line] ) || Line <- Out].

format_script( ActScript ) ->
  {_, S} = lists:foldl( fun( Line, {N, S} ) ->
                          {N+1, io_lib:format( "~s~4.B  ~s~n", [S, N, Line] )}
                        end,
                        {1, []}, re:split( ActScript, "\n" ) ),
  S.