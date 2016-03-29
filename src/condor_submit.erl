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
%

%% @author Irina Guberman <irina.guberman@gmail.com>
%% @author Jörgen Brandt <brandjoe@hu-berlin.de>
%%
%% @doc Management Condor submit files.
%%
%% This utility module produces the content of a condor_submit file from 3
%% sources of possible inputs in the following order of priority if there is a
%% parameter key conflict:
%%
%% 1. Mandatory CF condor parameters: executable, log, error, output
%%
%% 2. Optional task-level condor parameters, i.e. universe, RequestMemory, RequestCpus
%%
%% 3. Optional default condor parameters, i.e. universe, should_transfer_files, etc.
%%
%% One exception is transfer_input_files: instead of choosing just one of the corresponding
%% lists, the lists are merged.  This way any custom input files can be specified as well.
%%
%% <i>
%% Is specifying custom input files actually a good idea?
%% (i)  It is a way to introduce a file avoiding to define it as a tasks
%%      argument. (It is a side-effect.)
%% (ii) Imho, the semantics of the workflow script should never change due to
%%      an annotation. Annotations should only be for giving additional
%%      info to speed up execution.
%% </i>
%%
%% Also, input file names are validated to be real files, therefore transfer_input_files param
%% is the only one expected to be a list of files rather than a complete string and it will
%% be converted back to comma-separated string once the validation and possibly merging is done
%%
%% <i>
%% Validation is not necessary here. Effi checks if all files exist, before it
%% starts and fails if something is missing. It doesn't depend on Condor submit
%% info, though. Representing lists in the Condor submit file as lists in the
%% submit map is an excellent idea though.
%% </i>
%%
%% Mandatory condor parameters that aren't provided in either of the sources of
%% possible inputs will be filled in with VANILLA_DEFAULTS.

-module(condor_submit).

-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).
-endif.

% API
-export( [generate_condor_submit/1, generate_condor_submit/2,
          generate_condor_submit/3] ).

% provisionary export to make all module functions, their prototypes and
% documentations visible in the docs.
-export( [validate_input_files/1, combine_params/2, input_files_to_cs_string/1,
          format_submit/1] ).

-define(VANILLA_DEFAULTS,
  #{universe => "vanilla",
    should_transfer_files => "YES",
    run_as_owner => "True",                 % is this one really necessary?
    when_to_transfer_output => "ON_EXIT"}).

%%
%% This method expects 1 argument
%% 1. Mandatory CF-generated condor parameters, like 'executable', 'log', etc.
%% All the missing necessary parameters like 'universe' will be filled default values
generate_condor_submit(CFParams) -> generate_condor_submit(CFParams, #{}).
%%
%% This method expects 2 arguments:
%% 1. Mandatory CF-generated condor parameters, like 'executable', 'log', etc.
%% 2. declared default condor parameters
%%
generate_condor_submit(#{ executable := Executable,
  log := Log,
  output := Output,
  error := Error,
  initialdir := InitialDir} = CFParams,
    DeclaredParams) when
  is_list(Executable), length(Executable) > 0,
  is_list(Log), length(Log) > 0,
  is_list(Output), length(Output) > 0,
  is_list(Error), length(Error) > 0,
  is_list(InitialDir), length(InitialDir) > 0,
  is_map(DeclaredParams) ->
  CondorParams0  = combine_params(CFParams, DeclaredParams),
  CondorParams = maps:merge(?VANILLA_DEFAULTS, CondorParams0),
  format_submit(CondorParams).

%%
%% This method expects 3 arguments:
%% 1. Mandatory CF-generated condor parameters, like 'executable', 'log', etc.
%% 2. declared default condor parameters
%% 3. declared task-level condor parameters (any task-level parameters take precedence over defaults)
%%
generate_condor_submit(CFParams, DeclaredDefaultParams, DeclaredTaskParams)
  when is_map(DeclaredDefaultParams), is_map(DeclaredTaskParams)->
  DefaultParams = combine_params(DeclaredTaskParams, DeclaredDefaultParams),
  generate_condor_submit(CFParams, DefaultParams).


%% INTERNAL methods


%% @doc Extracts the transfer_input_files field from a Condor submit map and
%%      checks if all listed files exist.
%%
%%      Returns a Condor submit map having a non-empty transfer_input_files
%%      field or none at all. Throws an error if an input file does not exist.
%%
-spec validate_input_files( Params ) -> #{atom() => string() | [string()]}
when Params :: #{atom() => string() | [string()]}.

validate_input_files( #{ transfer_input_files := [] } = Params) ->
  maps:remove(transfer_input_files, Params);

validate_input_files( #{ transfer_input_files := InputFiles} = Params)
  when is_list(InputFiles) ->
  case lists:all(fun(Elem) -> filelib:is_file(Elem) end, InputFiles) of
    true -> Params;
    false -> {error, io_lib:format("Invalid input file(s) in ~p!", [InputFiles])}
  end;

validate_input_files(#{} = Params) -> Params.


%% @doc Combines parameters from two Condor submit maps.
%%
%%      In compination, Params1 will take precedence over Params2. If there is
%%      tranfer_input_files parameter present in both it will merge the list of
%%      input files into one.
%%
-spec combine_params( Params1, Params2 ) -> #{atom() => string() | [string()]}
when Params1 :: #{atom() => string() | [string()]},
     Params2 :: #{atom() => string() | [string()]}.

combine_params( #{ transfer_input_files := InputFiles1 } = Params1,
                #{ transfer_input_files := InputFiles2 } = Params2 )
  when is_list(InputFiles1), is_list(InputFiles2) ->
  InputFiles = InputFiles1 ++ InputFiles2,
  combine_params(
    maps:put(transfer_input_files, InputFiles, Params1),
    maps:remove(transfer_input_files, Params2)
  );

combine_params(Params1, Params2) when is_map(Params1), is_map(Params2) ->
  maps:merge(validate_input_files(Params2), validate_input_files(Params1)).


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
-spec input_files_to_cs_string( CondorParams ) -> #{atom() => string()}
when CondorParams :: #{atom() => string() | [string()]}.

input_files_to_cs_string(#{transfer_input_files := InputFiles} = CondorParams) ->
  InputFilesCS = string:join(InputFiles, ", "),
  maps:put(transfer_input_files, InputFilesCS, CondorParams);

input_files_to_cs_string(CondorParams) -> CondorParams.

%% @doc Takes a Condor submit map and formats it as a binary.
%%
%%      Takes the Condor submit map and produces a string having 'key = value'
%%      format. The order of key-value pairs in the output binary is undefined.
%%      The resulting binary is a complete, valid condor submit file leaving it
%%      to the caller to write the content to disk.
%%
-spec format_submit( Condorparams0 ) -> binary()
when Condorparams0::#{atom() => string() | [string()]}.

format_submit(CondorParams0) ->
  CondorParams1 = input_files_to_cs_string(CondorParams0),
  LineSep = io_lib:nl(),
  SubmitFileStr = maps:fold(fun(K, V, Acc) -> [Acc, io_lib:format("~p = ~s", [K,V]), LineSep] end, "", CondorParams1),
  iolist_to_binary([LineSep, lists:flatten(SubmitFileStr), LineSep, "Queue", LineSep]).











-ifdef( TEST ).

-define (GOOD_CF_PARAMS,
  #{ executable => "do_something.sh",
    log => "job123.log",
    output => "job123.stdout",
    error => "job123.stderr",
    initialdir => ".",
    transfer_input_files => []}).

%%% For testing with condor create `do_something.sh` in the same dir where you run
%%% `condor_submit /tmp/cf_params_test
cf_params_test() ->
  CondorSubmitStr = generate_condor_submit(?GOOD_CF_PARAMS),
  file:write_file("/tmp/cf_params_test", [CondorSubmitStr]).
%%   condor_submit("/tmp/cf_params_test").

%%% If files at the end aren't deleted it produces a valid submitfile: /tmp/default_parameters_test
%%% If run with `condor_submit /tmp/default_parameters_test`
%%% Gives a `WARNING: the line 'docker_image = some_image:latest' was unused by condor_submit. Is it a typo?`
%%% Demonstrating that if some params or their combo are invalid condor will handle the errors/warnings
%%% So cuneiform doesn't have to yet it retains full power of condor submit
default_params_test() ->
  DefaultParams = #{ universe => "docker",
    docker_image => "some_image:latest",
    transfer_input_files => ["/tmp/d1.tmp", "/tmp/d2.tmp"],
    should_transfer_files => "NO" },

  CFParams = maps:put(transfer_input_files, ["/tmp/cf1.tmp", "/tmp/cf2.tmp", "/tmp/cf3.tmp"], ?GOOD_CF_PARAMS),

%%   generate_condor_submit(CFParams, DefaultParams),

  Files = maps:get(transfer_input_files, DefaultParams) ++ maps:get(transfer_input_files, CFParams),
  create_temp_files(Files),
  CondorSubmitStr = generate_condor_submit(CFParams, DefaultParams),
  file:write_file("/tmp/default_params_test", CondorSubmitStr).
%%   condor_submit("/tmp/default_params_test").

% task_params_test() ->
%   DefaultParams = #{ universe => "docker",
%     docker_image => "some_image:latest",
%     transfer_input_files => ["/tmp/d1.tmp", "/tmp/d2.tmp"],
%     should_transfer_files => "NO" },

%   TaskParams = #{ requestMemory => "256M",
%     transfer_input_files => ["/tmp/t1.tmp", "/tmp/t2.tmp"]},

%   CFParams = maps:put(transfer_input_files, ["/tmp/cf1.tmp", "/tmp/cf2.tmp", "/tmp/cf3.tmp"], ?GOOD_CF_PARAMS),

% %%   generate_condor_submit(CFParams, DefaultParams, TaskParams),

%   Files = maps:get(transfer_input_files, DefaultParams) ++
%     maps:get(transfer_input_files, TaskParams) ++
%     maps:get(transfer_input_files, CFParams),

%   CondorSubmitStr = generate_condor_submit(CFParams, DefaultParams, TaskParams),
%   file:write_file("/tmp/task_params_test_error", CondorSubmitStr),

%   create_temp_files(Files),
%   CondorSubmitStr = generate_condor_submit(CFParams, DefaultParams, TaskParams),
%   file:write_file("/tmp/task_params_test", CondorSubmitStr).
%%   condor_submit("/tmp/task_params_test").

create_temp_files(Files) when is_list(Files) ->
  lists:foreach(fun(F) -> file:write_file(F, "") end, Files).

%%% TODO: move out of here into CT, IMPROVE TEST COVERAGE, and uncomment condor_submits

%% DELETE to test with condor
%% delete_temp_files(Files) ->
%%   lists:foreach(fun(F) -> file:delete(F) end, Files).

%% condor_submit(CondorSubmitFile) ->
%%   Out = os:cmd(["condor_submit", CondorSubmitFile]),
%%   OutFile = io_lib:format("/tmp/~s_submit_result", [CondorSubmitFile]),
%%   file:write_file(OutFile, Out).

-endif.



















