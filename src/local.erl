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

-module( local ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).

-behaviour( cf_cre ).
-export( [init/1, handle_submit/6, stage/6] ).

-define( BASEDIR, "/tmp/cf" ).
-define( WORK, "work" ).
-define( REPO, "repo" ).

-spec create_basedir( Prefix::string(), I::pos_integer() ) -> iolist().

create_basedir( Prefix, I ) ->
  BaseDir = [Prefix, "-", integer_to_list( I )],
  case filelib:is_file( BaseDir ) of
    true  -> create_basedir( Prefix, I+1 );
    false ->
      case filelib:ensure_dir( [BaseDir, "/"] ) of
        {error, Reason} -> error( {Reason, ensure_dir, [BaseDir, "/"]} );
        ok              -> BaseDir
      end
  end.

-spec init( NSlot::pos_integer() ) -> {ok, {iolist(), pid()}}.

init( NSlot ) when is_integer( NSlot ), NSlot > 0 ->
  BaseDir = create_basedir( ?BASEDIR, 1 ),
  {ok, QueueRef} = gen_queue:start_link( NSlot ),
  {ok, {BaseDir, QueueRef}}.


-spec handle_submit( Lam, Fa, R, DataDir, LibMap, {BaseDir, QueueRef} ) -> ok
when Lam      :: cre:lam(),
     Fa       :: #{string() => [cre:str()]},
     R        :: pos_integer(),
     DataDir  :: string(),
     LibMap   :: #{cf_sem:lang() => [string()]},
     BaseDir  :: iolist(),
     QueueRef :: pid().

handle_submit( Lam, Fa, R, DataDir, LibMap, {BaseDir, QueueRef} )
when is_tuple( Lam ),
     is_map( Fa ),
     is_integer( R ), R > 0,
     is_list( DataDir ),
     is_map( LibMap ),
     is_list( BaseDir ),
     is_pid( QueueRef ) ->
  gen_server:cast( QueueRef, {request, self(),
                   {?MODULE, stage, [Lam, Fa, R, DataDir, LibMap, BaseDir]}} ).


-spec stage( Lam, Fa, R, DataDir, LibMap, BaseDir ) -> cre:response()
when Lam     :: cre:lam(),
     Fa      :: #{string() => [cre:str()]},
     R       :: pos_integer(),
     DataDir :: string(),
     LibMap  :: #{cf_sem:lang() => [string()]},
     BaseDir :: iolist().

stage( Lam={lam, _LamLine, _LamName, {sign, Lo, Li}, _Body},
       Fa, R, DataDir, LibMap, BaseDir )
when is_list( Lo ),
     is_list( Li ),
     is_map( Fa ),
     is_integer( R ), R > 0,
     is_list( DataDir ),
     is_map( LibMap ),
     is_list( BaseDir ) ->

  Dir = string:join( [BaseDir, ?WORK, integer_to_list( R )], "/" ),
  RepoDir = string:join( [BaseDir, ?REPO], "/" ),

  % create working directory
  case filelib:ensure_dir( [Dir, "/"] ) of
    {error, R1} -> error( {R1, ensure_dir, [Dir, "/"]} );
    ok          -> ok
  end,

  % resolve input files
  Triple1 = refactor:get_refactoring( Li, Fa, Dir, [DataDir, RepoDir], R ),
  {RefactorLst1, MissingLst1, Fa1} = Triple1,

  case MissingLst1 of
    [_|_] -> {failed, precond, R, MissingLst1};
    []    ->

      % link in input files
      refactor:apply_refactoring( RefactorLst1 ),

      % start effi
      case effi:check_run( Lam, Fa1, R, Dir, LibMap ) of

        {failed, R2, R, Data} -> {failed, R2, R, Data};

        {finished, Sum}       ->

          Ret1 = maps:get( ret, Sum ),

          % resolve output files
          Triple2 = refactor:get_refactoring( Lo, Ret1, RepoDir, [Dir], R ),
          {RefactorLst2, [], Ret2} = Triple2,

          % link out output files
          refactor:apply_refactoring( RefactorLst2 ),

          % update result map
          {finished, Sum#{ret => Ret2}}
      end
  end.