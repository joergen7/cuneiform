%% -*- erlang -*-
%%
%% A functional workflow language for large-scale scientific data analysis.
%%
%% Copyright 2015-2018 Jörgen Brandt
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @author Jörgen Brandt <joergen.brandt@onlinehome.de>
%% @version 3.0.1
%% @copyright 2015-2018 Jörgen Brandt
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

-module( cuneiform ).

-export( [main/1] ).

-define( VSN, "3.0.1" ).

%%====================================================================
%% Escript main function
%%====================================================================

main( Args ) ->

  % TODO: neutralize all configurations

  try

    case getopt:parse( get_optspec_lst(), Args ) of
  
      {error, R1} ->
        throw( {error, R1} );

      {ok, {OptLst, NonOptLst}} ->

        % break if version needs to be displayed
        case lists:member( version, OptLst ) of
          false -> ok;
          true  -> throw( version )
        end,

        % break if help needs to be displayed
        case lists:member( help, OptLst ) of
          false -> ok;
          true  -> throw( help )
        end,

        % override configuration files
        application:set_env( cf_client, global_file, undefined ),
        application:set_env( cf_client, user_file, undefined ),
        application:unset_env( cf_client, suppl_file ),
        application:set_env( cf_worker, global_file, undefined ),
        application:set_env( cf_worker, user_file, undefined ),
        application:unset_env( cf_worker, suppl_file ),


        % extract number of workers
        M1 =
          case lists:keyfind( n_wrk, 1, OptLst ) of
            false                 -> #{};
            {n_wrk, 0}            -> #{ n_wrk => <<"auto">> };
            {n_wrk, N} when N > 0 -> #{ n_wrk => N };
            A                     -> throw( {error, {invalid_arg, A}} )
          end,

        % extract working directory
        M2 =
          case lists:keyfind( wrk_dir, 1, OptLst ) of
            false        -> M1;
            {wrk_dir, W} -> M1#{ wrk_dir => W }
          end,

        % extract repository directory
        M3 =
          case lists:keyfind( repo_dir, 1, OptLst ) of
            false         -> M2;
            {repo_dir, R} -> M2#{ repo_dir => R }
          end,

        % extract data directory
        M4 =
          case lists:keyfind( data_dir, 1, OptLst ) of
            false         -> M3;
            {data_dir, D} -> M3#{ data_dir => D }
          end,

        % set flag maps
        ok = application:set_env( cf_client, flag_map, #{} ),
        ok = application:set_env( cf_worker, flag_map, M4 ),



        % start CRE application
        ok = cre:start(),

        % start worker application
        ok = cf_worker:start(),

        % start client application
        ok = cf_client:start(),

        case NonOptLst of
          []    -> throw( shell );
          [_|_] -> throw( {load, NonOptLst} )
        end

    end

  catch

    throw:version ->
      print_version();

    throw:help ->
      print_help();

    throw:shell ->
      ok = cuneiform_shell:shell( cf_client );

    throw:{load, FileLst} ->

      F =
        fun( File ) ->

          % collect reply list
          ReplyLst =
            case file:read_file( File ) of

              {error, R3} ->
                [{error, load, {File, R3}}];

              {ok, B} ->
                S = binary_to_list( B ),
                cuneiform_shell:shell_eval_oneshot( S )

            end,

          cuneiform_shell:process_reply_lst( ReplyLst, cf_client, silent )

        end,

      ok = lists:foreach( F, FileLst );

    throw:{error, Reason} ->
      ok = io:format( "~n~p~n", [Reason] )

  end.



%%====================================================================
%% Internal functions
%%====================================================================

get_optspec_lst() ->
  [
   {version,    $v, "version",    undefined, "Show cf_worker version."},
   {help,       $h, "help",       undefined, "Show command line options."},
   {n_wrk,      $n, "n_wrk",      integer,   "Number of worker processes to start. 0 means auto-detect available processors."},
   {wrk_dir,    $w, "wrk_dir",    binary,    "Working directory in which workers store temporary files."},
   {repo_dir,   $r, "repo_dir",   binary,    "Repository directory for intermediate and output data."},
   {data_dir,   $d, "data_dir",   binary,    "Data directory where input data is located."}
  ].

print_help() ->
  getopt:usage( get_optspec_lst(), "cf_worker" ).

print_version() ->
  io:format( "cuneiform ~s~n", [?VSN] ).