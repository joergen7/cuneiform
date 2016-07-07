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

start_link() ->
  {ok, Pid} = gen_event:start_link( {local, ?MODULE} ),
  gen_event:add_handler( ?MODULE, ?MODULE, [] ),
  {ok, Pid}.

add_ip( Ip ) when is_list( Ip ) ->
  gen_event:call( ?MODULE, ?MODULE, {add_ip, Ip} ).

notify( LogEntry ) ->
  gen_event:notify( ?MODULE, LogEntry ).

code_change( _OldVsn, State, _Extra ) -> {ok, State}.

handle_call( {add_ip, Ip}, IpLst ) -> {ok, ok, [Ip|IpLst]}.

init( _InitArgs ) -> {ok, []}.

terminate( _Arg, _State ) -> ok.

handle_info( _Info, State ) -> {ok, State}.

handle_event( LogEntry, IpLst ) ->

  F = fun( Ip ) ->

	  Url = io_lib:format( "http://~s:~p", [Ip, ?PORT] ),
	  Request = {Url, [], "application/json", jsone:encode( LogEntry )},

	  httpc:request( post, Request, [], [] )

	 end,

  lists:foreach( F, IpLst ),

  {ok, IpLst}.