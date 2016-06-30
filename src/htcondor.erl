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
%% @author Irina Guberman <irina.guberman@gmail.com>

-module( htcondor ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).
-author( "Irina Guberman <irina.guberman@gmail.com>" ).
-vsn( "2.2.1-snapshot" ).


-include( "cuneiform.hrl" ).

-behaviour( cf_cre ).

-export( [init/1, handle_submit/7, input_files_to_cs_string/1] ).

-define( BASEDIR, "/tmp/cf" ).


init( _ModArg ) ->
  BaseDir = local:create_basedir( ?BASEDIR, 1 ),

  error_logger:info_msg( io_lib:format( "Base directory: ~s.~n", [BaseDir] ) ),

  {ok, BaseDir}.

handle_submit( Lam={lam, _LamLine, _LamName, {sign, Lo, Li}, _Body}, Fa,
  DataDir, _UserInfo, R, LibMap, BaseDir ) ->

  Dir = local:create_workdir( BaseDir, ?WORK, R ),
  
  RepoDir = string:join( [BaseDir, ?REPO], "/" ),

  % resolve input files
  Triple1 = lib_refactor:get_refactoring( Li, Fa, Dir, [DataDir, RepoDir], R ),
  {RefactorLst1, MissingLst1, Fa1} = Triple1,

  case MissingLst1 of
    [_|_] -> {failed, precond, R, MissingLst1};
    []    ->

      % link in input files
      lib_refactor:apply_refactoring( RefactorLst1 ),


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
      SubmitStr = format_submit( SubmitMap ),
      ok = file:write_file( SubmitFile, SubmitStr ),
      error_logger:info_msg( "~s~n", [SubmitStr] ),

      % submit job
      case lib_os:cmd( string:join( ["condor_submit", SubmitFile], " " ), Dir ) of

        {error, B1} -> {failed, cre_error, R, B1};
        ok          ->

          % wait until the job terminates
          case lib_os:cmd( string:join( ["condor_wait", LogFile], " " ), Dir ) of

            {error, B2} -> {failed, cre_error, R, B2};
            ok          ->

              {ok, B} = file:read_file( EffiSumFile ),
              {ok, Tokens, _} = erl_scan:string( binary_to_list( B ) ),
              {ok, Sum} = erl_parse:parse_term( Tokens ),

              case maps:get( state, Sum ) of

                ok           ->

                  % resolve output files
                  RMap = maps:get( ret, Sum ),
                  {lam, _Line, _LamName, Sign, _Body} = Lam,
                  {sign, Lo, _Li} = Sign,
                  {RefactorLst, [], RMap1} = lib_refactor:get_refactoring( Lo, RMap, RepoDir, [Dir], R ),
                  ok = lib_refactor:apply_refactoring( RefactorLst ),

                  {finished, maps:put( ret, RMap1, Sum )};

                script_error ->

                  #{actscript := ActScript, out := Out} = Sum,
                  {failed, script_error, R, {ActScript, Out}};

                R1           ->

                  #{missing := MissingLst2} = Sum,
                  {failed, R1, R, MissingLst2}
              end
          end
      end
  end.



%% @doc Takes a Condor submit map and formats it as a binary.
%%
%%      Takes the Condor submit map and produces a string having 'key = value'
%%      format. The order of key-value pairs in the output binary is undefined.
%%      The resulting binary is a complete, valid condor submit file leaving it
%%      to the caller to write the content to disk.
%%
-spec format_submit( Condorparams0 ) -> binary()
when Condorparams0::#{atom() => iolist() | [iolist()]}.

format_submit(CondorParams0) ->
  CondorParams1 = input_files_to_cs_string(CondorParams0),
  LineSep = io_lib:nl(),
  SubmitFileStr = maps:fold(fun(K, V, Acc) -> [Acc, io_lib:format("~p = ~s", [K,V]), LineSep] end, "", CondorParams1),
  iolist_to_binary([LineSep, lists:flatten(SubmitFileStr), LineSep, "Queue", LineSep]).

%% @doc Converts the transfer_input_files field from a Condor submit map to a
%%      comma-separated string.
%%
%%      The only parameter that might come here as a list instead of complete
%%      cs-string is transfer_input_files because it might be generated by
%%      Cuneiform and it needs file validation. Therefore any declared
%%      transfer_input_files params need to be first parsed from cs-string
%%      into a list, and once they are validated and possibly merged, they are
%%      converted back to cs-string here.
%%
-spec input_files_to_cs_string( CondorParams ) -> #{atom() => iolist()}
when CondorParams :: #{atom() => iolist() | [iolist()]}.

input_files_to_cs_string( CondorParams = #{transfer_input_files := InputFiles} ) ->
  InputFilesCS = string:join(InputFiles, ", "),
  maps:put(transfer_input_files, InputFilesCS, CondorParams);

input_files_to_cs_string(CondorParams) -> CondorParams.


