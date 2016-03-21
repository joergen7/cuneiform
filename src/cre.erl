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
%% ```+-------+   +-------+
%%    | Query |   | Query |
%%    +-------+   +-------+
%%            \   /
%%           +-----+
%%           | CRE |
%%           +-----+'''


-module( cre ).
-author( "Jorgen Brandt <brandjoe@hu-berlin.de>" ).
-behaviour( gen_server ).

%% =============================================================================
%% Function Exports
%% =============================================================================

-export( [start_link/0, submit/2] ).

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


-callback init() -> {ok, State::term()}.

-callback handle_submit( Lam, Fa, R, DataDir, ModState ) -> ok
when Lam      :: lam(),
     Fa       :: #{string() => [str()]},
     R        :: pos_integer(),
     DataDir  :: string(),
     ModState :: term().

%% =============================================================================
%% Type Definitions
%% =============================================================================

-type str()       :: {str, S::string()}.

-type lam()       :: {lam, LamLine::pos_integer(), Name::string(),
                           S::sign(), B::forbody()}.

-type sign()      :: {sign, Lo::[param()], Li::[param()]}.

-type param()     :: {param, M::name(), Pl::boolean()}.

-type name()      :: {name, N::string(), Pf::boolean()}.

-type forbody()   :: {forbody, L::lang(), S::string()}.

-type lang()      :: bash | python | r.

-type fut()       :: {fut, LamName::string(), R::pos_integer(),
                           Lo::[param()]}.

-type app()       :: {app, AppLine::pos_integer(), C::pos_integer(),
                           Lambda::lam(), Fa::#{string() => [str()]}}.

-type ckey()      :: {lam(), #{string() => [str()]}, string()}.

-type response()  :: {failed, pos_integer(), atom(), term()}
                   | {finished, #{atom() => term()}}.

-type cre_state() :: {Mod::atom(),
                      SubscrMap::#{pos_integer() => sets:set( pid() )},
                      ReplyMap::#{pos_integer() => response()},
                      Cache::#{ckey() => fut()},
                      R::pos_integer(),
                      ModState::term()}.

-type submit()    :: {submit, app(), string()}.

%% =============================================================================
%% Generic Server Functions
%% =============================================================================

code_change( _OldVsn, State, _Extra ) -> {ok, State}.
handle_cast( Request, _State ) -> error( {bad_request, Request} ).
terminate( _Reason, _State ) -> ok.

%% @doc Generates the initial state of the CRE.

-spec init( [] ) -> {ok, cre_state()}.

init( [] ) ->
  Mod       = local,      % CRE callback module implementing the stage function
  SubscrMap = #{},        % mapping of a future to a set of subscriber pids
  ReplyMap  = #{},        % mapping of a future to a response
  Cache     = #{},        % cache mapping a cache key to a future
  R         = 1,          % next id

  % initialize CRE
  {ok, ModState} = apply( Mod, init, [] ),

  {ok, {Mod, SubscrMap, ReplyMap, Cache, R, ModState}}.

%% @doc On receiving a call containing a subission, a future is generated and
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
-spec handle_call( Request, From, State ) -> {reply, fut(), cre_state()}
when Request :: submit(),
     From    :: {pid(), term()},
     State   :: cre_state().

handle_call( {submit, App, DataDir}, {Pid, _Tag}, {Mod, SubscrMap, ReplyMap, Cache, R, ModState} ) ->

  {app, _AppLine, _Channel, Lam, Fa} = App,
  {lam, _LamLine, LamName, {sign, Lo, _Li}, _Body} = Lam,

  % construct cache key
  Ckey = {Lam, Fa, DataDir},

  case maps:is_key( Ckey, Cache ) of

    false ->

      % create new future
      Fut = {fut, LamName, R, Lo},

      % start process
      apply( Mod, handle_submit, [Lam, Fa, R, DataDir, ModState] ),

      SubscrMap1 = SubscrMap#{R => sets:from_list( [Pid] )},
      Cache1 = Cache#{Ckey => Fut},
      R1 = R+1,

      {reply, Fut, {Mod, SubscrMap1, ReplyMap, Cache1, R1, ModState}};

    true ->

      % retrieve future from cache
      Fut = maps:get( Ckey, Cache ),

      {fut, _, S, _} = Fut,
      #{S := SubscrSet} = SubscrMap,

      SubscrMap1 = SubscrMap#{S => sets:add_element( Pid, SubscrSet )},

      case maps:is_key( S, ReplyMap ) of
        false -> ok;
        true  -> Pid ! maps:get( S, ReplyMap )
      end,

      {reply, Fut, {Mod, SubscrMap1, ReplyMap, Cache, R, ModState}}
  end.

%% Info Handler %%

-spec handle_info( Info, State ) -> {noreply, cre_state()}
when Info  :: response(),
     State :: cre_state().

handle_info( Info={failed, Reason, S, Data}, {Mod, SubscrMap, ReplyMap, Cache, R, ModState} ) ->

  % retrieve subscriber set
  #{S := SubscrSet} = SubscrMap,

  % notify subscribers
  lists:foreach( fun( Subscr ) ->
                   Subscr ! {failed, Reason, S, Data}
                 end,
                 sets:to_list( SubscrSet ) ),

  ReplyMap1 = ReplyMap#{S => Info},

  {noreply, {Mod, SubscrMap, ReplyMap1, Cache, R, ModState}};

handle_info( Info={finished, Sum}, {Mod, SubscrMap, ReplyMap, Cache, R, ModState} ) ->

  % retrieve subscriber set
  #{id := S} = Sum,
  #{S := SubscrSet} = SubscrMap,

  % notify subscribers
  lists:foreach( fun( Subscr ) ->
                   Subscr ! {finished, Sum}
                 end,
                 sets:to_list( SubscrSet ) ),


  ReplyMap1 = ReplyMap#{S => Info},

  {noreply, {Mod, SubscrMap, ReplyMap1, Cache, R, ModState}}.

%% =============================================================================
%% API Functions
%% =============================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, Error}
when Error :: {already_started, pid()} | term().

start_link() ->
  gen_server:start_link( {local, ?MODULE}, ?MODULE, [], [] ).


-spec submit( App::app(), DataDir::string() ) -> fut().

submit( App, DataDir ) ->
  gen_server:call( ?MODULE, {submit, App, DataDir} ).

%% =============================================================================
%% Internal Functions
%% =============================================================================


-ifdef( TEST ).


-endif.