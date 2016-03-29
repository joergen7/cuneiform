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

  _Dir = local:create_workdir( BaseDir, ?WORK, R ),
  
  _RepoDir = string:join( [BaseDir, ?REPO], "/" ),

  SubmitStr = "", % TODO

  Port = open_port( {spawn, "condor_submit -terse -"},
                    [exit_status,
                     stderr_to_stdout,
                     binary,
                     {cd, "."}, % TODO
                     {line, 1024}] ),

  true = port_command( Port, SubmitStr ),

  receive

    % TODO

    Msg -> error( {bad_msg, Msg} )
  end.