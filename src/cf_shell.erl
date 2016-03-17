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

-export( [server/0] ).

%% =============================================================================
%% Type Definitions
%% =============================================================================


%% =============================================================================
%% Macro Definitions
%% =============================================================================

-define( VSN, "2.2.0" ).
-define( BUILD, "2016-03-17" ).

-define( RED( Str ), "\e[31m" ++ Str ++ "\e[0m" ).
-define( GRN( Str ), "\e[1;32m" ++ Str ++ "\e[0m" ).
-define( YLW( Str ), "\e[1;33m" ++ Str ++ "\e[0m" ).
-define( BLU( Str ), "\e[1;34m" ++ Str ++ "\e[0m" ).

-define( PROMPT, "> " ).







%% =============================================================================
%% Internal Functions
%% =============================================================================


server() ->
  process_flag( trap_exit, true ),
  io:format( "~s", [get_banner()] ),
  server_loop( #{}, #{} ).

server_loop( Rho, Gamma ) ->

  {Query, DRho, DGamma} = read_expression( ?PROMPT ),

  Rho1 = maps:merge( Rho, DRho ),
  Gamma1 = maps:merge( Gamma, DGamma ),
  case Query of

    undef -> server_loop( Rho1, Gamma1 );
    _     ->
      cuneiform:reduce( Query, Rho, Gamma, "." ),
      server_loop( Rho, Gamma )

  end.



read_expression( Prompt ) ->
  Read = fun() ->
           io:format( Prompt ),
           Ret = read(),
           exit( Ret )
         end,
  Rdr = spawn_link( Read ),
  read_expression_1( Rdr ).

read_expression_1( Rdr ) ->
  receive
    {'EXIT', Rdr, Ret} -> Ret;
    Msg                -> error( {bad_msg, Msg} )
  end.

read() ->
  S = io:get_line( "" ),
  {ok, TokenLst, _} = cf_scan:string( S ),
  {ok, Ret} = cf_parse:parse( TokenLst ),
  Ret.



get_banner() ->
  string:join(
    ["            ___",
     "           @@WB      Cuneiform",
     "          @@E_____   "++?VSN++" "++?BUILD,
     "     _g@@@@@WWWWWWL",
     "   g@@#*`3@B         Type "++?GRN( "help;" )++" for usage info.",
     "  @@P    3@B",
     "  @N____ 3@B         Home: "++?BLU( "http://www.cuneiform-lang.org" ),
     "  \"W@@@WF3@B",
     "", ""
    ], "\n" ).

