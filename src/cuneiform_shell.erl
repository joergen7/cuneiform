%% -*- erlang -*-
%%
%% Cuneiform: A Functional Language for Large-Scale Scientific Data Analysis
%%
%% Copyright 2013-2017 Jörgen Brandt
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

%% @author Jörgen Brandt <joergen.brandt@onlinehome.de>


-module( cuneiform_shell ).

-export( [shell/0] ).


-define( BUILD, "2017-12-13" ).
-define( VSN, "3.0.0" ).

-define( RED( Str ), "\e[31m" ++ Str ++ "\e[0m" ).
-define( BRED( Str ), "\e[1;31m" ++ Str ++ "\e[0m" ).
-define( GRN( Str ), "\e[32m" ++ Str ++ "\e[0m" ).
-define( YLW( Str ), "\e[33m" ++ Str ++ "\e[0m" ).
-define( BYLW( Str ), "\e[1;33m" ++ Str ++ "\e[0m" ).
-define( BLU( Str ), "\e[1;34m" ++ Str ++ "\e[0m" ).


-spec shell() -> ok.

shell() ->
  io:format( "~s~n~n~n", [get_banner()] ),
  shell_loop().


-spec shell_loop() -> ok.
      shell_loop() -> shell_loop( 1, "", [], {[], [], [], []} ).

-spec shell_loop( Line, StringBuf, TokenBuf, ParseState ) -> ok
when Line       :: pos_integer(),
     StringBuf  :: string(),
     TokenBuf   :: [{atom(), pos_integer(), string()}],
     ParseState :: {[_], [_], [_], [_]}.

shell_loop( Line, StringBuf, TokenBuf, ParseState )
when is_integer( Line ), Line > 0,
     is_list( StringBuf ),
     is_tuple( ParseState ) ->

  {Line1, StringBuf1, TokenBuf1, ParseState1} =
    case is_ready( StringBuf ) of
      
      true ->
        {ok, TLst, L} = cuneiform_scan:string( StringBuf, Line ),
        case lists:reverse( TLst ) of

          [{A, _, _}|_] when A =:= semicolon orelse A =:= body ->
            {ok, D} = cuneiform_parse:parse( TokenBuf++TLst ),
            U = cuneiform_parse:join_stat( ParseState, D ),
            case U of

              {_, _, _, []} ->
                {L, "", [], U};

              {L1, L2, DefLst, [E|_]} -> % TODO: lists:foreach instead of just first
                Closure = cuneiform_parse:create_closure( DefLst, E ),
                Result = cre_client:eval( cf_client, Closure ),
                io:format( "~p~n", [Result] ),
                {L, "", [], {L1, L2, DefLst, []}}

            end;

          _ ->
            {L, "", TokenBuf++TLst, ParseState}

        end;



      false ->
        {Line, StringBuf, TokenBuf, ParseState}

    end,

  io:format( "{~p, ~p, ~p}~n", [StringBuf1, TokenBuf1, ParseState1] ),

  Prompt =
    case {StringBuf1, TokenBuf1} of
      {"", []} -> io_lib:format( "~p> ", [Line1] );
      _        -> ""
    end,
  
  case io:get_line( Prompt ) of
    "quit\n" -> ok;
    Delta    -> shell_loop( Line1, StringBuf1++Delta, TokenBuf1, ParseState1 )
  end.



-spec get_banner() -> string().

get_banner() ->
  string:join(
    ["            ___",
     "           @@WB      Cuneiform",
     "          @@E_____   version "++?VSN++"    build "++?BUILD,
     "     _g@@@@@WWWWWWL",
     "   g@@#*`3@B         "++?YLW( "Type " )++?BYLW( "help" )++?YLW( " for usage info." ),
     "  @@P    3@B              "            ++?BYLW( "quit" )++?YLW( " to exit shell." ),
     "  @N____ 3@B",
     "  \"W@@@WF3@B         "++?BLU( "http://www.cuneiform-lang.org" )
], "\n" ).


-spec is_ready( Buf :: string() ) -> boolean().
      is_ready( Buf )             -> is_ready( Buf, true ).

-spec is_ready( Buf :: string(), S :: boolean() ) -> boolean().

is_ready( [], S )                -> S;
is_ready( [$*, ${|Rest], true )  -> is_ready( Rest, false );
is_ready( [$}, $*|Rest], false ) -> is_ready( Rest, true );
is_ready( [_|Rest], S )          -> is_ready( Rest, S ).