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
-vsn( "2.2.0-snapshot" ).


-include( "cuneiform.hrl" ).

-behaviour( cf_cre ).

-export( [init/1, handle_submit/6] ).


init( _ModArg ) ->
  BaseDir = local:create_basedir( ?BASEDIR, 1 ),
  {ok, BaseDir}.

handle_submit( Lam, Fa, R, DataDir, LibMap, BaseDir ) ->
  CreRef = self(),
  _Pid = spawn_link( fun() -> CreRef ! stage( Lam, Fa, R, DataDir, LibMap, BaseDir ) end ),
  ok.

  

stage( Lam={lam, _LamLine, _LamName, {sign, Lo, Li}, _Body}, Fa, R, DataDir,
       LibMap, BaseDir ) ->

  Dir = local:create_workdir( BaseDir, ?WORK, R ),
  
  RepoDir = string:join( [BaseDir, ?REPO], "/" ),

  % resolve input files
  Triple1 = refactor:get_refactoring( Li, Fa, Dir, [DataDir, RepoDir], R ),
  {RefactorLst1, MissingLst1, Fa1} = Triple1,

  case MissingLst1 of
    [_|_] -> {failed, precond, R, MissingLst1};
    []    ->

      % link in input files
      refactor:apply_refactoring( RefactorLst1 ),


      LogFile = string:join( [Dir, "_log.txt"], "/" ),
      OutputFile = string:join( [Dir, "_output.txt"], "/" ),
      SubmitFile = string:join( [Dir, "_job.sub"], "/" ),
      EffiSumFile = string:join( [Dir, "_summary.effi"], "/" ),
      EffiRequestFile = string:join( [Dir, "_request.effi"], "/" ),

      SubmitMap = #{universe                => "VANILLA",
                    executable              => "/usr/local/bin/effi",
                    arguments               => string:join( [EffiRequestFile,
                                                             EffiSumFile], " " ),
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

      % write Effi submit file
      EffiRequest = {Lam, Fa1, R, LibMap},
      EffiRequestStr = io_lib:format( "~p.~n", [EffiRequest] ),
      ok = file:write_file( EffiRequestFile, EffiRequestStr ),

      % write HTCondor submit file
      SubmitStr = condor_submit:format_submit( SubmitMap ),
      ok = file:write_file( SubmitFile, SubmitStr ),
      error_logger:info_msg( "~s~n", [SubmitStr] ),

      % submit job
      _Response1 = os:cmd( string:join( ["condor_submit", SubmitFile], " " ) ),
      
      % wait until the job terminates
      _Response2 = os:cmd( string:join( ["condor_wait", LogFile], " " ) ),

      {ok, B} = file:read_file( EffiSumFile ),
      {ok, Tokens, _} = erl_scan:string( binary_to_list( B ) ),
      {ok, Sum} = erl_parse:parse_term( Tokens ),

      case maps:get( state, Sum ) of

        ok           ->

          % resolve output files
          RMap = maps:get( ret, Sum ),
          {lam, _Line, _LamName, Sign, _Body} = Lam,
          {sign, Lo, _Li} = Sign,
          {RefactorLst, [], RMap1} = refactor:get_refactoring( Lo, RMap, RepoDir, [Dir], R ),
          ok = refactor:apply_refactoring( RefactorLst ),

          {finished, maps:put( ret, RMap1, Sum )};



        script_error ->
          #{actscript := ActScript, out := Out} = Sum,
          {failed, script_error, R, {ActScript, Out}};

        R1           ->
          #{missing := MissingLst2} = Sum,
          {failed, R1, R, MissingLst2}
      end
  end.




