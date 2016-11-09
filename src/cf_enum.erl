-module( cf_enum ).

-export( [enum/1] ).

-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).
-endif.



enum( AppLst ) when is_list( AppLst ) ->
  lists:flatmap( fun enum/1, AppLst );

enum( {app, AppLine, C, {lam, LamLine, LamName, {sign, Lo, Li}, Body}, Fa} ) ->

  F = fun( Fa1 ) ->
        Sign = {sign, Lo, Li},
        Lam = {lam, LamLine, LamName, Sign, Body},
        {app, AppLine, C, Lam, Fa1}
      end,

  N = enum_len( Li, Fa ),
  Fa1Lst = enum_fixpnt( N, Li, Fa, [] ),

  [F( Fa1 )|| Fa1 <- Fa1Lst].


enum_fixpnt( 1, [], #{}, #{} ) ->
  [#{}];

enum_fixpnt( 0, _, _, Acc ) ->
  Acc;

enum_fixpnt( I, Li, Fa, Acc ) ->
  Fa1 = get_instance( I, Li, Fa, #{} ),
  enum_fixpnt( I-1, Li, Fa, [Fa1|Acc] ).




%% @doc Returns a list with the lengths of each input parameter in a given
%%      signature.

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
        Acc1#{ N => NewValue }
      end,

  NewAcc = lists:foldl( F, Acc, Lc ),

  get_instance( I div OrigLen, T, Fa, NewAcc ).



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

% Li = [{param, {name, "a", false}, false}],



-endif.