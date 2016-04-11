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

-module( lib_os ).
-vsn( "2.2.0-snapshot" ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).

-export( [cmd/2] ).

-define( BUF_SIZE, 1024 ).




-spec cmd( Cmd::string(), Dir::string() ) -> ok | {error, binary()}.

cmd( Cmd, Dir ) when is_list( Cmd ), is_list( Dir ) ->

  Port = open_port( {spawn, Cmd},
                    [exit_status,
                     stderr_to_stdout,
                     binary,
                     {cd, Dir},
                     {line, ?BUF_SIZE}] ),

  listen_port( Port, <<>> ).



  

-spec listen_port( Port::port(), OutAcc::binary() ) -> ok | {error, binary()}.

listen_port( Port, OutAcc ) when is_port( Port ), is_binary( OutAcc ) ->

  receive

    % no line feed, buffer line and continue
    {Port, {data, {noeol, PartLine}}} ->
      OutAcc1 = <<OutAcc/binary, PartLine/binary>>,
      listen_port( Port, OutAcc1 );

    % line feed encountered
    {Port, {data, {eol, PartLine}}} ->
      OutAcc1 = <<OutAcc/binary, PartLine/binary, "\n">>,
      listen_port( Port, OutAcc1 );

    % process succeeded
    {Port, {exit_status, 0}} ->
      ok;

    % process failed
    {Port, {exit_status, _}} ->
      {error, OutAcc};

    % if nothing matches, raise error
    Msg ->
      error( {bad_msg, Msg} )

  end.
