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

-module( logmgr ).

-behaviour( gen_event ).

-export( [start_link/0, add_ip/1, notify/1] ).
-export( [code_change/3, handle_call/2, init/1, terminate/2, handle_event/2,
          handle_info/2] ).

-define( PORT, 8080 ).
-define( VSN, <<"cf2.0">> ).

-record( mod_state, {ip_lst = [], session} ).

start_link() ->
  {ok, Pid} = gen_event:start_link( {local, ?MODULE} ),
  gen_event:add_handler( ?MODULE, ?MODULE, [] ),
  {ok, Pid}.

add_ip( Ip ) when is_list( Ip ) ->
  gen_event:call( ?MODULE, ?MODULE, {add_ip, Ip} ).

notify( LogEntry ) ->
  gen_event:notify( ?MODULE, LogEntry ).

code_change( _OldVsn, State, _Extra ) -> {ok, State}.

handle_call( {add_ip, Ip}, State = #mod_state{ ip_lst = IpLst } ) ->
  {ok, ok, State#mod_state{ ip_lst = [Ip|IpLst] }}.

init( _InitArgs ) ->

  SessionId = integer_to_binary( rand:uniform( 1000000000000 ) ),
  Tstart    = trunc( os:system_time()/1000000 ),

	Session = #{ id     => SessionId,
	             tstart => Tstart },

  {ok, #mod_state{ session = Session }}.

terminate( _Arg, _State ) -> ok.

handle_info( _Info, State ) -> {ok, State}.

handle_event( LogEntry, State = #mod_state{ ip_lst = IpLst, session = Session } ) ->

  io:format( "logmgr received an event.~n" ),

  F = fun( Ip ) ->


	  Url = lists:flatten( io_lib:format( "http://~s:~p", [Ip, ?PORT] ) ),
	  Request = {Url, [], "application/json", to_json( LogEntry, Session )},

    io:format( "Sending ~p to ~s~n", [LogEntry, Url] ),
	  X = httpc:request( post, Request, [], [] ),

	  io:format( "~p", [X] )

	 end,

  lists:foreach( F, IpLst ),

  {ok, State}.

to_json( {started, R, LamName}, Session ) ->

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

  {ok, Host} = inet:gethostname(),

  #{ id     := Id,
     state  := ok,
     lam    := Lam,
     tstart := Tstart,
     tdur   := Tdur, 
     out    := Out } = Sum,

  {lam, _, LamName, _, _} = Lam,

  Info = #{ tstart => Tstart, tdur => Tdur, out => Out },

  jsone:encode( #{ vsn      => ?VSN,
                   session  => Session,
                   msg_type => invoc,
                   data     => #{ host_name => list_to_binary( Host ), 
                                  id        => Id,
                                  status    => ok,
                                  lam_name  => list_to_binary( LamName ),
                                  info      => Info } } ).