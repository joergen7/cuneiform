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

-module( cf_enum ).
-author( "Jörgen Brandt <brandjoe@hu-berlin.de>" ).

-export( [enum/1] ).

-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).
-endif.



enum( AppLst ) when is_list( AppLst ) ->
  lists:flatmap( fun enum/1, AppLst );

enum( {app, AppLine, C, {lam, LamLine, LamName, {sign, Lo, Li}, Body}, Fa} ) ->

  Li1 = remove_correl( Li ),

  F = fun( Fa1 ) ->
        Sign = {sign, Lo, Li1},
        Lam = {lam, LamLine, LamName, Sign, Body},
        {app, AppLine, C, Lam, Fa1}
      end,

  N = enum_len( Li, Fa ),
  Fa1Lst = enum_fixpnt( N-1, lists:reverse( Li ), Fa, [] ),


  [F( Fa1 )|| Fa1 <- Fa1Lst].


enum_fixpnt( -1, _, _, Acc ) ->
  Acc;

enum_fixpnt( I, Li, Fa, Acc ) ->
  Fa1 = get_instance( I, Li, Fa, #{} ),
  enum_fixpnt( I-1, Li, Fa, [Fa1|Acc] ).




-spec enum_len( Li, Fa ) -> pos_integer()
when Li ::  [cf_sem:inparam()],
     Fa ::  #{string() => [cf_sem:expr()]}.

enum_len( Li, Fa ) ->

  F = fun( {param, _, true}, Acc ) ->
           Acc;
         ( {param, {name, N, _}, false}, Acc ) ->
           Len = length( maps:get( N, Fa ) ),
           Len*Acc;
         ( {correl, [{name, N, _}|_]}, Acc ) ->
           Len = length( maps:get( N, Fa ) ),
           Len*Acc
      end,

  lists:foldl( F, 1, Li ).


get_instance( _, [], _, Acc ) ->
  Acc;

get_instance( I, [{param, {name, N, _}, true}|T], Fa, Acc ) ->
  #{ N := Value } = Fa,
  get_instance( I, T, Fa, Acc#{ N => Value } );

get_instance( I, [{param, {name, N, _}, false}|T], Fa, Acc ) ->
  #{ N := OrigValue } = Fa,
  OrigLen = length( OrigValue ),
  J = ( I rem OrigLen )+1,
  NewValue = [lists:nth( J, OrigValue )],
  get_instance( I div OrigLen, T, Fa, Acc#{ N => NewValue } );

get_instance( I, [{correl, Lc=[{name, N, _}|_]}|T], Fa, Acc ) ->
  
  #{ N := Repres } = Fa,
  OrigLen = length( Repres ),
  J = ( I rem OrigLen )+1,

  F = fun( {name, N1, _}, Acc1 ) ->
        #{ N1 := OrigValue } = Fa,
        NewValue = [lists:nth( J, OrigValue )],
        Acc1#{ N1 => NewValue }
      end,

  NewAcc = lists:foldl( F, Acc, Lc ),

  get_instance( I div OrigLen, T, Fa, NewAcc ).

remove_correl( Li ) ->

  F = fun( P={param, _, _}, Acc ) -> Acc++[P];
         ( {correl, Lc}, Acc ) -> Acc++[{param, N, false} || N <- Lc]
      end,

  lists:foldl( F, [], Li ).



-ifdef( TEST ).

enum_empty_input_param_creates_single_instance_test() ->
  Li = [],
  Lo = [{param, {name, "out", false}, false}],
  Fa = #{},
  Sign = {sign, Lo, Li},
  Body = {forbody, bash, "blub"},
  Lam = {lam, 20, "f", Sign, Body},
  App = {app, 10, 1, Lam, Fa},

  ?assertEqual( [App], enum( [App] ) ).

enum_single_input_param_single_value_creates_single_instance_test() ->
  Li = [{param, {name, "a", false}, false}],
  Lo = [{param, {name, "out", false}, false}],
  Fa = #{"a" => [{str, "A"}]},
  Sign = {sign, Lo, Li},
  Body = {forbody, bash, "blub"},
  Lam = {lam, 20, "f", Sign, Body},
  App = {app, 10, 1, Lam, Fa},

  ?assertEqual( [App], enum( [App] ) ).

-endif.