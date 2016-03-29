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

-module( condor ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).

-include( "cuneiform.hrl" ).

-behaviour( cf_cre ).

-export( [init/1, handle_submit/6] ).


init( _ModArg ) ->

  BaseDir = local:create_basedir( ?BASEDIR, 1 ),
  {ok, BaseDir}.

handle_submit( _Lam, _Fa, R, _DataDir, _LibMap, BaseDir ) ->

  Dir = local:create_workdir( BaseDir, ?WORK, R ),
  
  _RepoDir = string:join( [BaseDir, ?REPO], "/" ),

  LogFile = string:join( [Dir, "_log.txt"], "/" ),
  SumFile = string:join( [Dir, "_summary.txt"], "/" ),
  OutputFile = string:join( [Dir, "_output.txt"], "/" ),

  SubmitMap = #{universe                => "VANILLA",
                executable              => "/usr/local/bin/effi",
                arguments               => string:join( ["_request.txt",
                                                         SumFile], " " ),
                should_transfer_files   => "IF_NEEDED",
                when_to_transfer_output => "ON_EXIT",
                log                     => LogFile,
                output                  => OutputFile,
                environment             => ["\"",
                                            string:join( ["HOME=.",
                                                          "PATH=/bin:/usr/bin:/usr/local/bin"], " " ),
                                            "\""],
                initialdir              => Dir
              },

  SubmitStr = condor_submit:format_submit( SubmitMap ),
  error_logger:info_msg( "~s~n", [SubmitStr] ),

  SubmitFile = string:join( [Dir, "_job.sub"], "/" ),

  ok = file:write_file( SubmitFile, SubmitStr ),

  _Response = os:cmd( string:join( ["condor_submit", SubmitFile], " " ) ),
  
  CreRef = self(),
  F = fun() -> condor_wait( CreRef, R, LogFile, SumFile, OutputFile ) end,
  _Pid = spawn_link( F ),

  ok.

  

condor_wait( CreRef, R, LogFile, SumFile, OutputFile ) ->

  % wait until the job terminates
  _Response = os:cmd( string:join( ["condor_wait", LogFile], " " ) ),

  case filelib:is_file( SumFile ) of

    false ->
      {ok, Out} = file:read_file( OutputFile ),
      CreRef ! {failed, script_error, R, {"", re:split( Out, "\\n" )}}; % TODO: extract actual script

    true  ->
      {ok, B} = file:read_file( SumFile ),
      {ok, Tokens, _} = erl_scan:string( binary_to_list( B ) ),
      {ok, Sum} = erl_parse:parse_term( Tokens ),
      CreRef ! {finished, Sum}
  end.



