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

%% @author Jörgen Brandt <brandjoe@hu-berlin.de>

-module( cf_shell ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).
-vsn( "2.2.0-snapshot" ).


%% =============================================================================
%% Function Exports
%% =============================================================================

-export( [server/1] ).

%% =============================================================================
%% Includes
%% =============================================================================

-include( "cuneiform.hrl" ).

%% =============================================================================
%% Macro Definitions
%% =============================================================================

-define( PROMPT, "> " ).

%% =============================================================================
%% Internal Functions
%% =============================================================================

-spec server( Cwd::string() ) -> ok.

server( Cwd ) ->
  io:format( "~s~n~n~n", [get_banner()] ),
  server_loop( #{}, #{}, Cwd ).

-spec server_loop( Rho, Gamma, Cwd ) -> ok
when Rho   :: #{string() => [cuneiform:expr()]},
     Gamma :: #{string() => fun()},
     Cwd   :: string().

server_loop( Rho, Gamma, Cwd ) ->
  case read_expression( ?PROMPT ) of
    {ctl, quit}                 ->
      ok;
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
      S = cuneiform:format_error( ErrorInfo ),
      io:format( "~s~n", [S] ),
      server_loop( Rho, Gamma, Cwd );
    {ok, {Query, DRho, DGamma}} ->
      Rho1 = maps:merge( Rho, DRho ),
      Gamma1 = maps:merge( Gamma, DGamma ),
      case Query of
        undef -> server_loop( Rho1, Gamma1, Cwd );
        _     ->
          try cuneiform:reduce( Query, Rho1, Gamma1, "." ) of
            X -> io:format( "~s~n", [cuneiform:format_result( X )] )
          catch
            throw:T -> io:format( "~s~n", [cuneiform:format_error( T )] )
          end,
          server_loop( Rho1, Gamma1, Cwd )
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



-spec get_help() -> iolist().

get_help() ->
  string:join(
    [?YLW( "help" )++"  show this usage info",
     ?YLW( "state" )++" show variable bindings",
     ?YLW( "tasks" )++" show task definitions",
     ?YLW( "cwd" )++"   current working directory",
     ?YLW( "quit" )++"  quit the shell"
    ], "\n" ).


-spec get_banner() -> iolist().

get_banner() ->
  string:join(
    ["            ___",
     "           @@WB      Cuneiform",
     "          @@E_____   "++cuneiform:get_vsn()++" "++?BUILD,
     "     _g@@@@@WWWWWWL",
     "   g@@#*`3@B         "++?YLW( "Type " )++?BYLW( "help" )++?YLW( " for usage info." ),
     "  @@P    3@B",
     "  @N____ 3@B         "++?BLU( "http://www.cuneiform-lang.org" ),
     "  \"W@@@WF3@B"
    ], "\n" ).


