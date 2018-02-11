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
%% @version 3.0.0
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

-define( VSN, "3.0.0" ).


%%====================================================================
%% Escript main function
%%====================================================================

main( Args ) ->

  try

    case getopt:parse( get_optspec_lst(), Args ) of
  
      {error, R1} ->
        error( R1 );

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

        % extract supplement configuration file
        SupplFile =
          case lists:keyfind( suppl_file, 1, OptLst ) of
            false            -> undefined;
            {suppl_file, S1} -> S1
          end,

        % set supplement file
        ok = application:set_env( cf_client, suppl_file, SupplFile ),

        % extract CRE node name
        M1 =
          case lists:keyfind( cre_node, 1, OptLst ) of
            false               -> #{};
            {cre_node, CreNode} -> #{ cre_node => CreNode }
          end,

        % set flag map
        ok = application:set_env( cf_client, flag_map, M1 ),

        % start client service
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

              {error, R2} ->
                [{error, load, {File, R2}}];

              {ok, B} ->
                S = binary_to_list( B ),
                cuneiform_shell:shell_eval_oneshot( S )

            end,

          % print reply list on screen
          ok = cuneiform_shell:process_reply_lst( ReplyLst, cf_client )

        end,

      ok = lists:foreach( F, FileLst )


  end.

%%====================================================================
%% Internal functions
%%====================================================================

get_optspec_lst() ->
  [
   {version,    $v, "version",    undefined, "Show cf_worker version."},
   {help,       $h, "help",       undefined, "Show command line options."},
   {suppl_file, $s, "suppl_file", binary,    "Supplementary configuration file."},
   {cre_node,   $c, "cre_node",   binary,    "Erlang node running the CRE application."}

  ].

print_help() ->
  getopt:usage( get_optspec_lst(), "cf_worker" ).

print_version() ->
  io:format( "cuneiform ~s~n", [?VSN] ).