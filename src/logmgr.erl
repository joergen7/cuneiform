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

%% @doc Handles log messages and 

%% @author Jörgen Brandt <brandjoe@hu-berlin.de>

-module( logmgr ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).

-behaviour( gen_event ).

%% =============================================================================
%% Function Exports
%% =============================================================================

%% API
-export( [start_link/0, add_ip/1, notify/1] ).

%% gen_event behaviour callbacks
-export( [code_change/3, handle_call/2, init/1, terminate/2, handle_event/2,
          handle_info/2] ).

%% =============================================================================
%% Includes and Definitions
%% =============================================================================

-define( PORT, 8080 ).
-define( PATH, "submit" ).
-define( VSN, <<"cf2.0">> ).

%% =============================================================================
%% Type Definitions
%% =============================================================================

-record( mod_state, {ip_lst = [], session} ).

%% =============================================================================
%% Generic Event Functions
%% =============================================================================

%% init/1
%% @doc generates the initial state of the event manager,
%% consisting of the session data (id and start timestamp)
init( _InitArgs ) ->

  SessionId = integer_to_binary( rand:uniform( 1000000000000 ) ),
  Tstart    = trunc( os:system_time()/1000000 ),

  Session = #{ id     => SessionId,
               tstart => Tstart },

  {ok, #mod_state{ session = Session }}.

%% handle_event/2
%
handle_event( LogEntry, State = #mod_state{ ip_lst = IpLst, session = Session } ) ->

  F = fun( Ip ) ->


    Url = lists:flatten( io_lib:format( "http://~s:~p/~s", [Ip, ?PORT, ?PATH] ) ),
    Request = {Url, [], "application/json", to_json( LogEntry, Session )},

    io:format( "Sending ~w to ~s~n~p", [element( 1, LogEntry ), Url, to_json( LogEntry, Session )] ),

    X = httpc:request( post, Request, [], [{sync, true}] ),

    io:format( "~nResult ~p~n", [X] )

   end,

  lists:foreach( F, IpLst ),

  {ok, State}.

%% handle_call/2
%
handle_call( {add_ip, Ip}, State = #mod_state{ ip_lst = IpLst } ) ->
  {ok, ok, State#mod_state{ ip_lst = [Ip|IpLst] }}.

%% handle_info/2
%
handle_info( _Info, State ) -> {ok, State}.

%% code_change/3
%
code_change( _OldVsn, State, _Extra ) -> {ok, State}.

%% terminate/2
%
terminate( _Arg, _State ) -> ok.

%% =============================================================================
%% API Functions
%% =============================================================================

%% add_ip/1
%
add_ip( Ip ) when is_list( Ip ) ->
  gen_event:call( ?MODULE, ?MODULE, {add_ip, Ip} ).

%% start_link/0
%
start_link() ->
  {ok, Pid} = gen_event:start_link( {local, ?MODULE} ),
  gen_event:add_handler( ?MODULE, ?MODULE, [] ),
  {ok, Pid}.

%% notify/1
%% @doc use this function to submit log entries
notify( LogEntry ) ->
  gen_event:sync_notify( ?MODULE, LogEntry ).

%% =============================================================================
%% Non-exported Functions
%% =============================================================================

%% to_json/2
%
to_json( {started, R, LamName}, Session ) ->
  
  io:format("to json {started, ~p, ~p}~n", [R, LamName]),

  {ok, Host} = inet:gethostname(),

  jsone:encode( #{ vsn      => ?VSN,
  	                session  => Session,
  	                msg_type => invoc,
  	                data     => #{ host_name => list_to_binary( Host ), id => R, lam_name => list_to_binary( LamName ), status => started } } );

to_json( {failed, Reason, S, MissingLst}, Session )when is_list( MissingLst ) ->

  {ok, Host} = inet:gethostname(),

	jsone:encode( #{ vsn      => ?VSN,
		               session  => Session,
		               msg_type => invoc,
		               data     => #{ host_name  => list_to_binary( Host ), 
		                              id         => S,
		                              status     => error,
		                              lam_name   => undef,
		                              info       => #{ reason  => Reason,
		                                               missing => [list_to_binary( M ) || M <- MissingLst] } } } );

to_json( {failed, Reason, S, {ActScript, Out}}, Session ) ->

  {ok, Host} = inet:gethostname(),

	jsone:encode( #{ vsn      => ?VSN,
		               session  => Session,
		               msg_type => invoc,
		               data     => #{ host_name  => list_to_binary( Host ), 
		                              id         => S,
		                              status     => error,
		                              lam_name   => undef,
		                              info       => #{ reason     => Reason,
		                                               act_script => list_to_binary( ActScript ),
		                                               out        => Out } } } );


to_json( {finished, Sum}, Session ) ->

  io:format("to json {finished, ~p}~n", [maps:get(id, Sum)]),

  {ok, Host} = inet:gethostname(),

  #{ id     := Id,
     state  := ok,
     lam    := Lam,
     tstart := Tstart,
     tdur   := Tdur, 
     out    := Out,
     out_size_map := OutSizeMap,
     in_size_map := InSizeMap,
     profiling_results := ProfilingResults } = Sum,

  {lam, _, LamName, _, _} = Lam,

  Info = #{ tstart       => Tstart,
            tdur         => Tdur,
            out          => Out,
            out_size_map => OutSizeMap,
            in_size_map  => InSizeMap,
            profiling_results => ProfilingResults },

  jsone:encode( #{ vsn      => ?VSN,
                   session  => Session,
                   msg_type => invoc,
                   data     => #{ host_name => list_to_binary( Host ), 
                                  id        => Id,
                                  status    => ok,
                                  lam_name  => list_to_binary( LamName ),
                                  info      => Info } } ).