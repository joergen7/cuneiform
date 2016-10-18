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

%% @doc htcondor execution platform.

%% @author Jörgen Brandt <brandjoe@hu-berlin.de>
%% @author Irina Guberman <irina.guberman@gmail.com>

-module( htcondor ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).
-author( "Irina Guberman <irina.guberman@gmail.com>" ).
-vsn( "2.2.1-snapshot" ).

-behaviour( cf_cre ).

%% =============================================================================
%% Function Exports
%% =============================================================================

%% cf_cre behaviour callbacks
-export([init/1, handle_submit/8]).

%% API
-export( [input_files_to_cs_string/1] ).

%% =============================================================================
%% Includes and Definitions
%% =============================================================================

-include( "cuneiform.hrl" ).

-define( BASEDIR, "/tmp/cf" ).

%% =============================================================================
%% HTCondor Functions
%% =============================================================================

%% init/1
%
init( _ModArg ) ->
  BaseDir = local:create_basedir( ?BASEDIR, 1 ),

  error_logger:info_msg( io_lib:format( "Base directory: ~s.~n", [BaseDir] ) ),

  {ok, BaseDir}.

%% handle_submit/1
%% @doc the profiling_settings data structure is only used for determining whether profiling
%% is on or off. The profile file name is generated automatically. However, due to being a
%% callback implementation, this is useful (since for instance the local runtime environment
%% does use all of the profiling_settings attributes).
handle_submit( Lam={lam, _LamLine, _LamName, {sign, Lo, Li}, _Body}, Fa,
  DataDir, _UserInfo, R, LibMap, BaseDir, DoProf ) ->

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


      LogFile = filename:join( [Dir, "_log.txt"] ),
      OutputFile = filename:join( [Dir, "_output.txt"] ),
      SubmitFile = filename:join( [Dir, "_job.sub"] ),
      EffiSumFile = filename:join( [Dir, "_summary.effi"] ),
      EffiRequestFile = filename:join( [Dir, "_request.effi"] ),

      EffiArguments = case effi_profiling:is_on( DoProf ) of
        false -> 
          % If profiling is off, the parameters consist only of the mandatory requestfile and summary file
          string:join( [EffiRequestFile, EffiSumFile], " " );
        true -> 
          % Generate profile results file name, like the other file names above
          EffiProfileFile = filename:join( [Dir, "_profile.xml" ] ), 
          Prof = effi_profiling:get_profiling_settings( true, EffiProfileFile ),
          EffiProfArgs = effi_profiling:effi_arguments_for( Prof ),
          % Assemble the Effi parameters, including profiling options
          string:join( [EffiRequestFile, EffiSumFile, EffiProfArgs], " " )
      end,

      SubmitMap = #{universe                => "VANILLA",
                    executable              => "/usr/local/bin/effi",
                    arguments               => EffiArguments,
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


%% format_submit/1 
%%
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

%% input_files_to_cs_string/1 
%%
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


