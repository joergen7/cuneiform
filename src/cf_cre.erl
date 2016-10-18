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
%% @doc The Cuneiform Runtime Environment (CRE).
%% Each platform must implement the CRE callbacks, e.g., {@link local} and {@link htcondor}.
%% 
%% ```+-------+   +-------+
%%    | Query |   | Query |
%%    +-------+   +-------+
%%            \   /
%%           +-----+
%%           | CRE |
%%           +-----+'''


-module( cf_cre ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).

-behaviour( gen_server ).

%% =============================================================================
%% Function Exports
%% =============================================================================

-export( [start_link/3, submit/4] ).

-export( [code_change/3, handle_cast/2, handle_info/2, init/1, terminate/2,
          handle_call/3] ).

%% =============================================================================
%% Includes
%% =============================================================================

-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).
-endif.

%% =============================================================================
%% Callback Function Declarations
%% =============================================================================


-callback init( Arg::term() ) -> {ok, State::term()}.

-callback handle_submit( Lam, Fa, DataDir, UserInfo, R, LibMap, ModState, Prof ) ->
  {failed, Reason, S, Data} | {finished, Sum}
when Lam      :: cf_sem:lam(),
     Fa       :: #{string() => [cf_sem:str()]},
     DataDir  :: string(),
     UserInfo :: _,
     R        :: pos_integer(),
     LibMap   :: #{cf_sem:lang() => [string()]},
     ModState :: term(),
     Prof     :: effi_profiling:profilingsettings(),
     Reason   :: atom(),
     S        :: pos_integer(),
     Data     :: term(),
     Sum      :: map().

%% =============================================================================
%% Type Definitions
%% =============================================================================

-type ckey()      :: {cf_sem:lam(), #{string() => [cf_sem:str()]}, string()}.

-type response()  :: {failed, pos_integer(), atom(), term()}
                   | {finished, #{atom() => term()}}.

-type cre_state() :: {Mod::atom(),
                      SubscrMap::#{pos_integer() => sets:set( pid() )},
                      ReplyMap::#{pos_integer() => response()},
                      Cache::#{ckey() => cf_sem:fut()},
                      R::pos_integer(),
                      LibMap::#{cf_sem:lang() => [string()]},
                      ModState::term()}.

-type submit()    :: {submit, App::cf_sem:app(), DataDir::string(), UserInfo::_}.

%% =============================================================================
%% Generic Server Functions
%% =============================================================================

code_change( _OldVsn, State, _Extra ) -> {ok, State}.
handle_cast( Request, _State ) -> error( {bad_request, Request} ).
terminate( _Reason, _State ) -> ok.

%% @doc Generates the initial state of the CRE.

-spec init( {Mod, ModArg, LibMap} ) -> {ok, cre_state()}
when Mod    :: atom(),
     ModArg :: term(),
     LibMap :: #{cf_sem:lang() => [string()]}.

init( {Mod, ModArg, LibMap} )
when is_atom( Mod ), is_map( LibMap ) ->

  SubscrMap = #{},        % mapping of a future to a set of subscriber pids
  ReplyMap  = #{},        % mapping of a future to a response
  Cache     = #{},        % cache mapping a cache key to a future
  R         = 1,          % next id

  % initialize CRE
  {ok, ModState} = apply( Mod, init, [ModArg] ),

  {ok, {Mod, SubscrMap, ReplyMap, Cache, R, LibMap, ModState}}.

%% @doc On receiving a call containing a submission, a future is generated and
%%      returned.
%%
%%      When a submission request is received by the CRE, it returns a future by
%%      either creating a future, and spawning a corresponding job, or by
%%      returning a future from its cache, if a corresponding job has already
%%      been started.
%%
%%      Additionally, the source of the call is added to the subscriber list for
%%      the corresponding future. All subscribers to a future eventually
%%      receive a notification about the result of the submission.
%%
%%      If the future is served from the cache and a reply has already been
%%      received, the subscriber is immediately notified.
%%
-spec handle_call( Request, From, State ) -> {reply, cf_sem:fut(), cre_state()}
when Request :: submit(),
     From    :: {pid(), term()},
     State   :: cre_state().

handle_call( {submit, App, DataDir, UserInfo},
             {Pid, _Tag},
             {Mod, SubscrMap, ReplyMap, Cache, R, LibMap, ModState} )

when is_tuple( App ),
     is_list( DataDir ),
     is_pid( Pid ),
     is_atom( Mod ),
     is_map( SubscrMap ),
     is_map( ReplyMap ),
     is_map( Cache ),
     is_integer( R ), R > 0,
     is_map( LibMap ) ->

  {app, _AppLine, _Channel, Lam, Fa} = App,
  {lam, _LamLine, LamName, {sign, Lo, _Li}, _Body} = Lam,

  % construct cache key
  Ckey = {Lam, Fa, DataDir},

  case maps:is_key( Ckey, Cache ) of

    false ->

      % start process
      Runtime = self(),
      Prof = effi_profiling:get_profiling_settings( false, "<requestfile>_profile.xml" ),
      _Pid = spawn_link( fun() -> stage( Runtime, Lam, Fa, DataDir, UserInfo, Mod, R, LibMap, ModState, Prof ) end ),

      % create new future
      Fut = {fut, LamName, R, Lo},

      SubscrMap1 = SubscrMap#{R => sets:from_list( [Pid] )},
      Cache1 = Cache#{Ckey => Fut},
      R1 = R+1,

      logmgr:notify( {started, R, LamName} ),

      {reply, Fut, {Mod, SubscrMap1, ReplyMap, Cache1, R1, LibMap, ModState}};

    true ->

      % retrieve future from cache
      Fut = maps:get( Ckey, Cache ),

      {fut, _, S, _} = Fut,
      #{S := SubscrSet} = SubscrMap,

      SubscrMap1 = SubscrMap#{S => sets:add_element( Pid, SubscrSet )},

      case maps:is_key( S, ReplyMap ) of
        false -> ok;
        true  ->
          Reply = maps:get( S, ReplyMap ),
          Pid ! Reply,
          case erlang:function_exported(Mod, handle_cached, 2) of
            false -> ok;
            true -> apply(Mod, handle_cached, [ModState, Reply])
          end
      end,

      {reply, Fut, {Mod, SubscrMap1, ReplyMap, Cache, R, LibMap, ModState}}
  end;

handle_call( get_cache, _From,
             State={_Mod, _SubscrMap, _ReplyMap, Cache, _R, _LibMap, _ModState} )
when is_map( Cache ) ->
  {reply, Cache, State};

handle_call( get_modstate, _From,
             State={_Mod, _SubscrMap, _ReplyMap, _Cache, _R, _LibMap, ModState} ) ->
  {reply, ModState, State};

handle_call( get_replymap, _From,
             State={_Mod, _SubscrMap, ReplyMap, _Cache, _R, _LibMap, _ModState} )
when is_map( ReplyMap ) ->
  {reply, ReplyMap, State}.


%% Info Handler %%

-spec handle_info( Info, State ) -> {noreply, cre_state()}
when Info  :: response(),
     State :: cre_state().

handle_info( Info={failed, Reason, S, Data},
             {Mod, SubscrMap, ReplyMap, Cache, R, LibMap, ModState} )
when is_atom( Reason ),
     is_integer( S ), S > 0 ->

  % retrieve subscriber set
  #{S := SubscrSet} = SubscrMap,

  % notify subscribers
  lists:foreach( fun( Subscr ) ->
                   Subscr ! {failed, Reason, S, Data}
                 end,
                 sets:to_list( SubscrSet ) ),

  ReplyMap1 = ReplyMap#{S => Info},

  logmgr:notify( Info ),

  {noreply, {Mod, SubscrMap, ReplyMap1, Cache, R, LibMap, ModState}};

handle_info( Info={finished, Sum},
             {Mod, SubscrMap, ReplyMap, Cache, R, LibMap, ModState} ) ->

  % retrieve subscriber set
  #{id := S} = Sum,
  #{S := SubscrSet} = SubscrMap,

  % notify subscribers
  lists:foreach( fun( Subscr ) ->
                   Subscr ! {finished, Sum}
                 end,
                 sets:to_list( SubscrSet ) ),


  ReplyMap1 = ReplyMap#{S => Info},

  logmgr:notify( Info ),

  {noreply, {Mod, SubscrMap, ReplyMap1, Cache, R, LibMap, ModState}}.

%% =============================================================================
%% API Functions
%% =============================================================================

-spec start_link( Mod, ModArg, LibMap ) -> {ok, pid()} | ignore | {error, Error}
when Mod    :: atom(),
     ModArg :: term(),
     LibMap :: #{cf_sem:lang() => [string()]},
     Error  :: {already_started, pid()} | term().

start_link( Mod, ModArg, LibMap )
when is_atom( Mod ), is_map( LibMap ) ->
  gen_server:start_link( {local, cre}, ?MODULE, {Mod, ModArg, LibMap}, [] ).


-spec submit( Runtime, App, DataDir, UserInfo ) -> cf_sem:fut()
when Runtime  :: atom() | pid(),
     App      :: cf_sem:app(),
     DataDir  :: string(),
     UserInfo :: _.

submit( Runtime, App, DataDir, UserInfo )
when is_pid( Runtime ) orelse is_atom( Runtime ), is_tuple( App ), is_list( DataDir ) ->
  gen_server:call( Runtime, {submit, App, DataDir, UserInfo} ).

%% =============================================================================
%% Internal Functions
%% =============================================================================

stage( Runtime, Lam, Fa, DataDir, UserInfo, Mod, R, LibMap, ModState, Prof )
when is_pid( Runtime ) orelse is_atom( Runtime ),
     is_tuple( Lam ),
     is_map( Fa ),
     is_list( DataDir ),
     is_atom( Mod ),
     is_integer( R ), R > 0,
     is_map( LibMap ) ->

  Reply = apply( Mod, handle_submit, [Lam, Fa, DataDir, UserInfo, R, LibMap, ModState, Prof] ),
  Runtime ! Reply.


-ifdef( TEST ).


-endif.
