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

-module( cuneiform_parse_test ).

-include_lib( "eunit/include/eunit.hrl" ).

-import( cuneiform_parse, [parse/1] ).
-import( cuneiform_lang, [
                          str/2, var/2, file/2, true/1, false/1, cmp/3, conj/3,
                          disj/3, neg/2, cnd/4, lam_ntv/3, lam_ntv_arg/2,
                          t_str/0, t_file/0, app/3, r_var/2, t_fn/3, t_rcd/1,
                          t_arg/2, l_bash/0, lam_frn/6, t_lst/1, l_octave/0,
                          t_bool/0, l_perl/0, l_python/0, l_r/0, e_bind/2,
                          l_racket/0, fix/2, t_fn/4, rcd/2, r_rcd/1, r_bind/2,
                          proj/3, append/3, lst/3, isnil/2, for/4, fold/4,
                          assign/3
                         ] ).

parse_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"one variable definition",      fun one_variable_definition/0},
    {"two variable_definition",      fun two_variable_definition/0},
    {"one single import",            fun one_single_import/0},
    {"two single imports",           fun two_single_imports/0},
    {"import definition",            fun import_definition/0},
    {"variable",                     fun variable/0},
    {"string",                       fun string/0},
    {"integer",                      fun integer/0},
    {"file",                         fun file/0},
    {"true",                         fun true/0},
    {"false",                        fun false/0},
    {"compare string",               fun compare_string/0},
    {"conditional",                  fun conditional/0},
    {"conditional let then block",   fun conditional_let_then_block/0},
    {"conditional let else block",   fun conditional_let_else_block/0},
    {"conditional let both blocks",  fun conditional_let_both_blocks/0},
    {"negation",                     fun negation/0},
    {"conjunction",                  fun conjunction/0},
    {"disjunction",                  fun disjunction/0},
    {"no arg function",              fun no_arg_function/0},
    {"two arg function",             fun two_arg_function/0},
    {"foreign function bash",        fun foreign_function_bash/0},
    {"foreign function octave",      fun foreign_function_octave/0},
    {"foreign function perl",        fun foreign_function_perl/0},
    {"foreign function python",      fun foreign_function_python/0},
    {"foreign function r",           fun foreign_function_r/0},
    {"foreign function racket",      fun foreign_function_racket/0},
    {"const foreign function alias", fun const_foreign_function_alias/0},
    {"const native function alias",  fun const_native_function_alias/0},
    {"arg foreign function alias",   fun arg_foreign_function_alias/0},
    {"arg native function alias",    fun arg_native_function_alias/0},
    {"record pattern match",         fun record_pattern_match/0},
    {"record pattern multi match",   fun record_pattern_multi_match/0},
    {"record field access",          fun record_field_access/0},
    {"empty list",                   fun empty_list/0},
    {"isnil list",                   fun isnil_list/0},
    {"list append",                  fun list_append/0},
    {"const native function let",    fun const_native_function_let/0},
    {"arg native function let",      fun arg_native_function_let/0},
    {"latest let binds closest",     fun latest_let_binds_closest/0},
    {"map",                          fun map/0},
    {"map let",                      fun map_let/0},
    {"zip pair",                     fun zip_pair/0},
    {"zip triple",                   fun zip_triple/0},
    {"fold",                         fun fold/0},
    {"fold let",                     fun fold_let/0}
   ]
  }.

one_variable_definition() ->
  TokenLst = [{assign, 1, "let"}, {id, 1, "x"}, {colon, 1, ":"},
              {t_str, 1, "Str"}, {eq, 1, "="}, {strlit, 1, "bla"},
              {semicolon, 1, ";"},
              {id, 2, "x"}, {semicolon, 2, ";"}],
  ?assertEqual( {ok, {[],
                      [assign( 1, r_var( x, 'Str' ), str( 1, <<"bla">> ) )],
                      [var( 2, x )]}}, parse( TokenLst ) ).

two_variable_definition() ->
  TokenLst = [{assign, 1, "let"}, {id, 1, "x"}, {colon, 1, ":"},
              {t_str, 1, "Str"}, {eq, 1, "="}, {strlit, 1, "bla"},
              {semicolon, 1, ";"},
              {assign, 2, "let"}, {id, 2, "y"}, {colon, 2, ":"},
              {t_str, 2, "Str"}, {eq, 2, "="}, {strlit, 2, "blub"},
              {semicolon, 2, ";"}, {id, 3, "x"}, {semicolon, 3, ";"}],
  ?assertEqual( {ok, {[],
                      [assign( 1, r_var( x, t_str() ), str( 1, <<"bla">> ) ),
                       assign( 2, r_var( y, t_str() ), str( 2, <<"blub">> ) )],
                      [var( 3, x )]}}, parse( TokenLst ) ).

one_single_import() ->
  TokenLst = [{import, 1, "import"}, {filelit, 1, "a.cuf"}, {semicolon, 1, ";"},
              {strlit, 2, "bla"}, {semicolon, 2, ";"}],
  ?assertEqual( {ok, {[{import, 1, "a.cuf"}],
                      [],
                      [str( 2, <<"bla">> )]}}, parse( TokenLst ) ).

two_single_imports() ->
  TokenLst = [{import, 1, "import"}, {filelit, 1, "a.cuf"}, {semicolon, 1, ";"},
              {import, 2, "import"}, {filelit, 2, "b.cuf"}, {semicolon, 2, ";"},
              {strlit, 3, "bla"}, {semicolon, 3, ";"}],
  ?assertEqual( {ok, {[{import, 1, "a.cuf"}, {import, 2, "b.cuf"}],
                      [],
                      [str( 3, <<"bla">> )]}}, parse( TokenLst ) ).

import_definition() ->
  TokenLst = [{import, 1, "import"}, {filelit, 1, "a.cuf"}, {semicolon, 1, ";"},
              {assign, 2, "let"}, {id, 2, "x"}, {colon, 2, ":"},
              {t_str, 2, "Str"}, {eq, 2, "="}, {strlit, 2, "bla"},
              {semicolon, 2, ";"},
              {id, 3, "x"}, {semicolon, 3, ";"}],
  ?assertEqual( {ok, {[{import, 1, "a.cuf"}],
                      [assign( 2, r_var( x, t_str() ), str( 2, <<"bla">> ) )],
                      [var( 3, x )]}}, parse( TokenLst ) ).

variable() ->
  TokenLst = [{id, 1, "x"}, {semicolon, 1, ";"}],
  ?assertEqual( {ok, {[], [], [var( 1, x )]}}, parse( TokenLst ) ).

string() ->
  TokenLst = [{strlit, 1, "bla"}, {semicolon, 1, ";"}],
  ?assertEqual( {ok, {[], [], [str( 1, <<"bla">> )]}}, parse( TokenLst ) ).

integer() ->
  TokenLst = [{intlit, 1, "-5"}, {semicolon, 1, ";"}],
  ?assertEqual( {ok, {[], [], [str( 1, <<"-5">> )]}}, parse( TokenLst ) ).

file() ->
  TokenLst = [{filelit, 1, "blub.txt"}, {semicolon, 1, ";"}],
  ?assertEqual( {ok, {[], [], [file( 1, <<"blub.txt">> )]}}, parse( TokenLst ) ).

true() ->
  TokenLst = [{true, 1, "true"}, {semicolon, 1, ";"}],
  ?assertEqual( {ok, {[], [], [true( 1 )]}}, parse( TokenLst ) ).

false() ->
  TokenLst = [{false, 1, "false"}, {semicolon, 1, ";"}],
  ?assertEqual( {ok, {[], [], [false( 1 )]}}, parse( TokenLst ) ).

compare_string() ->
  TokenLst = [{lparen, 1, "("}, {strlit, 1, "bla"}, {cmp, 1, "=="},
              {strlit, 1, "blub"}, {rparen, 1, ")"}, {semicolon, 1, ";"}],
  ?assertEqual( {ok, {[], [], [cmp( 1, str( 1, <<"bla">> ), str( 1, <<"blub">> ) )]}},
                parse( TokenLst ) ).

conditional() ->
  TokenLst = [{cnd, 1, "if"},
              {true, 1, "true"},
              {then, 2, "then"},
              {strlit, 3, "bla"},
              {else, 4, "else"},
              {strlit, 5, "blub"},
              {halt, 6, "end"},
              {semicolon, 6, ";"}],
  E = cnd( 1, true( 1 ), str( 3, <<"bla">> ), str( 5, <<"blub">> ) ),
  ?assertEqual( {ok, {[], [], [E]}}, parse( TokenLst ) ).

conditional_let_then_block() ->
  TokenLst = [{cnd, 1, "if"},
              {true, 1, "true"},
              {then, 2, "then"},
              {assign, 3, "let"},
              {id, 3, "x"},
              {colon, 3, ":"},
              {t_str, 3, "Str"},
              {eq, 3, "="},
              {strlit, 3, "bla"},
              {semicolon, 3, ";"},
              {id, 4, "x"},
              {else, 5, "else"},
              {strlit, 6, "blub"},
              {halt, 7, "end"},
              {semicolon, 7, ";"}],
  A = app( 3,
           lam_ntv( 3, [lam_ntv_arg( x, t_str() )], var( 4, x ) ),
           [e_bind( x, str( 3, <<"bla">> ) )] ),
  E = cnd( 1, true( 1 ), A, str( 6, <<"blub">> ) ),
  ?assertEqual( {ok, {[], [], [E]}}, parse( TokenLst ) ).

conditional_let_else_block() ->
  TokenLst = [{cnd, 1, "if"},
              {true, 1, "true"},
              {then, 2, "then"},
              {strlit, 3, "bla"},
              {else, 4, "else"},
              {assign, 5, "let"},
              {id, 5, "x"},
              {colon, 5, ":"},
              {t_str, 5, "Str"},
              {eq, 5, "="},
              {strlit, 5, "blub"},
              {semicolon, 5, ";"},
              {id, 6, "x"},
              {halt, 7, "end"},
              {semicolon, 7, ";"}],
  A = app( 5,
           lam_ntv( 5, [lam_ntv_arg( x, t_str() )], var( 6, x ) ),
           [e_bind( x, str( 5, <<"blub">> ) )] ),
  E = cnd( 1, true( 1 ), str( 3, <<"bla">> ), A ),
  ?assertEqual( {ok, {[], [], [E]}}, parse( TokenLst ) ).

conditional_let_both_blocks() ->
  TokenLst = [{cnd, 1, "if"},
              {true, 1, "true"},
              {then, 2, "then"},
              {assign, 3, "let"},
              {id, 3, "x"},
              {colon, 3, ":"},
              {t_str, 3, "Str"},
              {eq, 3, "="},
              {strlit, 3, "bla"},
              {semicolon, 3, ";"},
              {id, 4, "x"},
              {else, 5, "else"},
              {assign, 6, "let"},
              {id, 6, "x"},
              {colon, 6, ":"},
              {t_str, 6, "Str"},
              {eq, 6, "="},
              {strlit, 6, "blub"},
              {semicolon, 6, ";"},
              {id, 7, "x"},
              {halt, 8, "end"},
              {semicolon, 8, ";"}],
  A1 = app( 3,
            lam_ntv( 3, [lam_ntv_arg( x, t_str() )], var( 4, x ) ),
            [e_bind( x, str( 3, <<"bla">> ) )] ),
  A2 = app( 6,
            lam_ntv( 6, [lam_ntv_arg( x, t_str() )], var( 7, x ) ),
            [e_bind( x, str( 6, <<"blub">> ) )] ),
  E = cnd( 1, true( 1 ), A1, A2 ),
  ?assertEqual( {ok, {[], [], [E]}}, parse( TokenLst ) ).

negation() ->
  TokenLst = [{neg, 1, "not"}, {false, 1, "false"}, {semicolon, 1, ";"}],
  ?assertEqual( {ok, {[], [], [neg( 1, false( 1 ) )]}}, parse( TokenLst ) ).

conjunction() ->
  TokenLst = [{lparen, 1, "("}, {true, 1, "true"}, {wedge, 1, "and"},
              {false, 1, "false"}, {rparen, 1, ")"}, {semicolon, 1, ";"}],
  ?assertEqual( {ok, {[], [], [conj( 1, true( 1 ), false( 1 ) )]}},
                parse( TokenLst ) ).

disjunction() ->
  TokenLst = [{lparen, 1, "("}, {true, 1, "true"}, {vee, 1, "or"},
              {false, 1, "false"}, {rparen, 1, ")"}, {semicolon, 1, ";"}],
  ?assertEqual( {ok, {[], [], [disj( 1, true( 1 ), false( 1 ) )]}},
                parse( TokenLst ) ).

no_arg_function() ->
  TokenLst = [{def, 1, "def"}, {id, 1, "f"}, {lparen, 1, "("},
              {rparen, 1, ")"}, {rarrow, 1, "->"}, {t_str, "1", "Str"},
              {lbrace, 1, "{"}, {strlit, 2, "blub"}, {rbrace, 3, "}"},
              {id, 5, "f"}, {lparen, 5, "("}, {rparen, 5, ")"}, {semicolon, 5, ";"}],
  E = app( 5, var( 5, f ), [] ),
  T = t_fn( ntv, [], t_str() ),
  F = fix( 1, lam_ntv( 1, [lam_ntv_arg( f, T )], str( 2, <<"blub">> ) ) ),
  R = r_var( f, T ),
  Def = assign( 1, R, F ),
  ?assertEqual( {ok, {[], [Def], [E]}}, parse( TokenLst ) ).

two_arg_function() ->
  TokenLst = [{def, 1, "def"}, {id, 1, "f"}, {lparen, 1, "("}, {id, 1, "a"},
              {colon, 1, ":"}, {t_str, 1, "Str"}, {comma, 1, ","},
              {id, 1, "b"}, {colon, 1, ":"}, {t_file, 1, "File"},
              {rparen, 1, ")"}, {rarrow, 1, "->"}, {t_str, 1, "Str"},
              {lbrace, 1, "{"}, {id, 2, "a"}, {rbrace, 3, "}"},
              {id, 5, "f"}, {lparen, 5, "("}, {id, 5, "a"}, {eq, 5, "="},
              {strlit, 5, "bla"}, {comma, 5, ","}, {id, 5, "b"}, {eq, 5, "="},
              {filelit, 5, "blub.txt"}, {rparen, 5, ")"}, {semicolon, 1, ";"}],
  E = app( 5, var( 5, f ), [e_bind( a, str( 5, <<"bla">> ) ),
                  e_bind( b, file( 5, <<"blub.txt">> ) )] ),
  T = t_fn( ntv, [t_arg( a, t_str() ), t_arg( b, t_file() )], t_str() ),
  F = fix( 1, lam_ntv( 1, [lam_ntv_arg( f, T ),
                        lam_ntv_arg( a, t_str() ),
                        lam_ntv_arg( b, t_file() )], var( 2, a ) ) ),
  R = r_var( f, T ),
  Def = assign( 1, R, F ),
  ?assertEqual( {ok, {[], [Def], [E]}}, parse( TokenLst ) ).

foreign_function_bash() ->
  TokenLst = [{def, 1, "def"},
              {id, 1, "f"},
              {lparen, 1, "("},
              {rparen, 1, ")"},
              {rarrow, 1, "->"},
              {ltag, 1, "<"},
              {id, 1, "out"}, {colon, 1, ":"}, {t_str, 1, "Str"},
              {rtag, 1, ">"},
              {in, 1, "in"}, {l_bash, 1, "Bash"}, {body, 1, "blablub"},
              {id, 5, "f"}, {lparen, 5, "("}, {rparen, 5, ")"}, {semicolon, 5, ";"}],
  E = app( 5, var( 5, f ), [] ),
  DefLst = [assign( 1, r_var( f, t_fn( frn, [], t_rcd( [t_arg( out, t_str() )] ) ) ),
             lam_frn( 1, f, [], t_rcd( [t_arg( out, t_str() )] ),
                      l_bash(), <<"blablub">> ) )],
  ?assertEqual( {ok, {[], DefLst, [E]}}, parse( TokenLst ) ).

foreign_function_octave() ->
  TokenLst = [{def, 1, "def"},
              {id, 1, "f"},
              {lparen, 1, "("},
              {rparen, 1, ")"},
              {rarrow, 1, "->"},
              {ltag, 1, "<"},
              {id, 1, "out1"}, {colon, 1, ":"}, {lsquarebr, 1, "["},
              {t_str, 1, "Str"}, {rsquarebr, 1, "]"}, {comma, 1, ","},
              {id, 1, "out2"}, {colon, 1, ":"}, {t_file, 1, "File"},
              {rtag, 1, ">"},
              {in, 1, "in"}, {l_octave, 1, "Octave"}, {body, 1, "blablub"},
              {id, 5, "f"}, {lparen, 5, "("}, {rparen, 5, ")"}, {semicolon, 5, ";"}],
  E = app( 5, var( 5, f ), [] ),
  DefLst = [assign( 1, r_var( f, t_fn( frn, [], t_rcd( [t_arg( out1, t_lst( t_str() ) ),
                                                 t_arg( out2, t_file() )] ) ) ),
             lam_frn( 1, f, [], t_rcd( [t_arg( out1, t_lst( t_str() ) ),
                                        t_arg( out2, t_file() )] ),
                      l_octave(), <<"blablub">> ) )],
  ?assertEqual( {ok, {[], DefLst, [E]}}, parse( TokenLst ) ).

foreign_function_perl() ->
  TokenLst = [{def, 1, "def"},
              {id, 1, "f"},
              {lparen, 1, "("},
              {rparen, 1, ")"},
              {rarrow, 1, "->"},
              {ltag, 1, "<"},
              {id, 1, "out1"}, {colon, 1, ":"}, {lsquarebr, 1, "["},
              {t_file, 1, "File"}, {rsquarebr, 1, "]"}, {comma, 1, ","},
              {id, 1, "out2"}, {colon, 1, ":"}, {t_bool, 1, "Bool"},
              {rtag, 1, ">"},
              {in, 1, "in"}, {l_perl, 1, "Perl"}, {body, 1, "blablub"},
              {id, 5, "f"}, {lparen, 5, "("}, {rparen, 5, ")"}, {semicolon, 5, ";"}],
  E = app( 5, var( 5, f ), [] ),
  DefLst = [assign( 1, r_var( f, t_fn( frn, [], t_rcd( [t_arg( out1, t_lst( t_file() ) ),
                                                 t_arg( out2, t_bool() )] ) ) ),
             lam_frn( 1, f, [], t_rcd( [t_arg( out1, t_lst( t_file() ) ),
                                        t_arg( out2, t_bool() )] ),
                      l_perl(), <<"blablub">> ) )],
  ?assertEqual( {ok, {[], DefLst, [E]}}, parse( TokenLst ) ).

foreign_function_python() ->
  TokenLst = [{def, 1, "def"},
              {id, 1, "f"},
              {lparen, 1, "("},
              {rparen, 1, ")"},
              {rarrow, 1, "->"},
              {ltag, 1, "<"},
              {id, 1, "out"}, {colon, 1, ":"}, {lsquarebr, 1, "["},
              {t_bool, 1, "Bool"}, {rsquarebr, 1, "]"},
              {rtag, 1, ">"},
              {in, 1, "in"}, {l_python, 1, "Python"}, {body, 1, "blablub"},
              {id, 5, "f"}, {lparen, 5, "("}, {rparen, 5, ")"}, {semicolon, 5, ";"}],
  E = app( 5, var( 5, f ), [] ),
  DefLst = [assign( 1, r_var( f, t_fn( frn, [], t_rcd( [t_arg( out, t_lst( t_bool() ) )] ) ) ),
             lam_frn( 1, f, [], t_rcd( [t_arg( out, t_lst( t_bool() ) )] ),
                      l_python(), <<"blablub">> ) )],
  ?assertEqual( {ok, {[], DefLst, [E]}}, parse( TokenLst ) ).

foreign_function_r() ->
  TokenLst = [{def, 1, "def"},
              {id, 1, "f"},
              {lparen, 1, "("},
              {id, 1, "x"}, {colon, 1, ":"}, {t_str, 1, "Str"},
              {rparen, 1, ")"},
              {rarrow, 1, "->"},
              {ltag, 1, "<"},
              {id, 1, "out"}, {colon, 1, ":"}, {t_str, 1, "Str"},
              {rtag, 1, ">"},
              {in, 1, "in"}, {l_r, 1, "R"}, {body, 1, "blablub"},
              {id, 5, "f"}, {lparen, 5, "("}, {id, 5, "x"}, {eq, 5, "="},
              {id, 5, "x"}, {rparen, 5, ")"}, {semicolon, 5, ";"}],
  E = app( 5, var( 5, f ), [e_bind( x, var( 5, x ) )] ),
  DefLst = [assign( 1, r_var( f, t_fn( frn, [t_arg( x, t_str() )], t_rcd( [t_arg( out, t_str() )] ) ) ),
             lam_frn( 1, f, [t_arg( x, t_str() )], t_rcd( [t_arg( out, t_str() )] ),
                      l_r(), <<"blablub">> ) )],
  ?assertEqual( {ok, {[], DefLst, [E]}}, parse( TokenLst ) ).

foreign_function_racket() ->
  TokenLst = [{def, 1, "def"},
              {id, 1, "f"},
              {lparen, 1, "("},
              {id, 1, "x"}, {colon, 1, ":"}, {t_str, 1, "Str"}, {comma, 1, ","},
              {id, 1, "y"}, {colon, 1, ":"}, {t_file, 1, "File"},
              {rparen, 1, ")"},
              {rarrow, 1, "->"},
              {ltag, 1, "<"},
              {id, 1, "out"}, {colon, 1, ":"}, {t_str, 1, "Str"},
              {rtag, 1, ">"},
              {in, 1, "in"}, {l_racket, 1, "Racket"}, {body, 1, "blablub"},
              {id, 5, "f"}, {lparen, 5, "("}, {id, 5, "x"}, {eq, 5, "="},
              {id, 5, "x"}, {comma, 5, ","}, 
              {id, 5, "y"}, {eq, 5, "="}, {id, 5, "y"},
              {rparen, 5, ")"}, {semicolon, 5, ";"}],
  E = app( 5, var( 5, f ), [e_bind( x, var( 5, x ) ), e_bind( y, var( 5, y ) )] ),
  DefLst = [assign( 1, r_var( f, t_fn( frn, [t_arg( x, t_str() ), t_arg( y, t_file() )], t_rcd( [t_arg( out, t_str() )] ) ) ),
             lam_frn( 1, f, [t_arg( x, t_str() ), t_arg( y, t_file() )], t_rcd( [t_arg( out, t_str() )] ),
                      l_racket(), <<"blablub">> ) )],
  ?assertEqual( {ok, {[], DefLst, [E]}}, parse( TokenLst ) ).

const_foreign_function_alias() ->
  TokenLst = [{def, 1, "def"},
              {id, 1, "f"},
              {lparen, 1, "("},
              {rparen, 1, ")"},
              {rarrow, 1, "->"},
              {ltag, 1, "<"},
              {id, 1, "x"},
              {colon, 1, ":"},
              {t_str, 1, "Str"},
              {rtag, 1, ">"},
              {in, 1, "in"},
              {l_bash, 1, "Bash"},
              {body, 1, "blub"},
              {assign, 2, "let"},
              {id, 2, "g"},
              {colon, 2, ":"},
              {t_fn_frn, 2, "Frn"},
              {lparen, 2, "("},
              {rparen, 2, ")"},
              {rarrow, 2, "->"},
              {ltag, 2, "<"},
              {id, 2, "x"},
              {colon, 2, ":"},
              {t_str, 2, "Str"},
              {rtag, 2, ">"},
              {eq, 2, "="},
              {id, 2, "f"},
              {semicolon, 2, ";"},
              {id, 3, "g"},
              {lparen, 3, "("},
              {rparen, 3, ")"},
              {semicolon, 3, ";"}],
  RetType = t_rcd( [t_arg( x, t_str() )] ),
  T = t_fn( frn, [], RetType ),
  F = lam_frn( 1, f, [], RetType, l_bash(), <<"blub">> ),
  G = var( 2, f ),
  H = app( 3, var( 3, g ), [] ),
  RF = r_var( f, T ),
  RG = r_var( g, T ),
  DefF = assign( 1, RF, F ),
  DefG = assign( 2, RG, G ),
  ?assertEqual( {ok, {[], [DefF, DefG], [H]}}, parse( TokenLst ) ).

const_native_function_alias() ->
  TokenLst = [{def, 1, "def"},
              {id, 1, "f"},
              {lparen, 1, "("},
              {rparen, 1, ")"},
              {rarrow, 1, "->"},
              {t_str, 1, "Str"},
              {lbrace, 1, "{"},
              {strlit, 2, "blub"},
              {rbrace, 3, "}"},
              {assign, 5, "let"},
              {id, 5, "g"},
              {colon, 5, ":"},
              {t_fn_ntv, 5, "Ntv"},
              {lparen, 5, "("},
              {rparen, 5, ")"},
              {rarrow, 5, "->"},
              {t_str, 5, "Str"},
              {eq, 5, "="},
              {id, 5, "f"},
              {semicolon, 5, ";"},
              {id, 6, "g"},
              {lparen, 6, "("},
              {rparen, 6, ")"},
              {semicolon, 6, ";"}],
  RetType = t_str(),
  T = t_fn( ntv, [], RetType ),
  F = fix( 1, lam_ntv( 1, [lam_ntv_arg( f, T )], str( 2, <<"blub">> ) ) ),
  G = var( 5, f ),
  H = app( 6, var( 6, g ), [] ),
  RF = r_var( f, T ),
  RG = r_var( g, T ),
  DefF = assign( 1, RF, F ),
  DefG = assign( 5, RG, G ),
  ?assertEqual( {ok, {[], [DefF, DefG], [H]}}, parse( TokenLst ) ).

arg_foreign_function_alias() ->
  TokenLst = [{def, 1, "def"},
              {id, 1, "f"},
              {lparen, 1, "("},
              {id, 1, "a"},
              {colon, 1, ":"},
              {t_str, 1, "Str"},
              {rparen, 1, ")"},
              {rarrow, 1, "->"},
              {ltag, 1, "<"},
              {id, 1, "x"},
              {colon, 1, ":"},
              {t_str, 1, "Str"},
              {rtag, 1, ">"},
              {in, 1, "in"},
              {l_bash, 1, "Bash"},
              {body, 1, "blub"},
              {assign, 2, "let"},
              {id, 2, "g"},
              {colon, 2, ":"},
              {t_fn_frn, 2, "Frn"},
              {lparen, 2, "("},
              {id, 2, "a"},
              {colon, 2, ":"},
              {t_str, 2, "Str"},
              {rparen, 2, ")"},
              {rarrow, 2, "->"},
              {ltag, 2, "<"},
              {id, 2, "x"},
              {colon, 2, ":"},
              {t_str, 2, "Str"},
              {rtag, 2, ">"},
              {eq, 2, "="},
              {id, 2, "f"},
              {semicolon, 2, ";"},
              {id, 3, "g"},
              {lparen, 3, "("},
              {id, 3, "a"},
              {eq, 3, "="},
              {strlit, 3, "a"},
              {rparen, 3, ")"},
              {semicolon, 3, ";"}],
  RetType = t_rcd( [t_arg( x, t_str() )] ),
  T = t_fn( frn, [t_arg( a, t_str() )], RetType ),
  F = lam_frn( 1, f, [t_arg( a, t_str() )], RetType, l_bash(), <<"blub">> ),
  G = var( 2, f ),
  H = app( 3, var( 3, g ), [e_bind( a, str( 3, <<"a">> ) )] ),
  RF = r_var( f, T ),
  RG = r_var( g, T ),
  DefF = assign( 1, RF, F ),
  DefG = assign( 2, RG, G ),
  ?assertEqual( {ok, {[], [DefF, DefG], [H]}}, parse( TokenLst ) ).

arg_native_function_alias() ->
  TokenLst = [{def, 1, "def"},
              {id, 1, "f"},
              {lparen, 1, "("},
              {id, 1, "a"},
              {colon, 1, ":"},
              {t_str, 1, "Str"},
              {rparen, 1, ")"},
              {rarrow, 1, "->"},
              {t_str, 1, "Str"},
              {lbrace, 1, "{"},
              {strlit, 2, "blub"},
              {rbrace, 3, "}"},
              {assign, 5, "let"},
              {id, 5, "g"},
              {colon, 5, ":"},
              {t_fn_ntv, 5, "Ntv"},
              {lparen, 5, "("},
              {id, 5, "a"},
              {colon, 5, ":"},
              {t_str, 5, "Str"},
              {rparen, 5, ")"},
              {rarrow, 5, "->"},
              {t_str, 5, "Str"},
              {eq, 5, "="},
              {id, 5, "f"},
              {semicolon, 5, ";"},
              {id, 6, "g"},
              {lparen, 6, "("},
              {id, 6, "a"},
              {eq, 6, "="},
              {strlit, 6, "a"},
              {rparen, 6, ")"},
              {semicolon, 6, ";"}],
  RetType = t_str(),
  T = t_fn( ntv, [t_arg( a, t_str() )], RetType ),
  F = fix( 1, lam_ntv( 1, [lam_ntv_arg( f, T ),
                           lam_ntv_arg( a, t_str() )], str( 2, <<"blub">> ) ) ),
  G = var( 5, f ),
  H = app( 6, var( 6, g ), [e_bind( a, str( 6, <<"a">> ) )] ),
  RF = r_var( f, T ),
  RG = r_var( g, T ),
  DefF = assign( 1, RF, F ),
  DefG = assign( 5, RG, G ),
  ?assertEqual( {ok, {[], [DefF, DefG], [H]}}, parse( TokenLst ) ).

record_pattern_match() ->
  TokenLst = [{assign, 1, "let"},
              {ltag, 1, "<"},
              {id, 1, "a"},
              {eq, 1, "="},
              {id, 1, "x"},
              {colon, 1, ":"},
              {t_str, 1, "Str"},
              {rtag, 1, ">"},
              {eq, 1, "="},
              {ltag, 1, "<"},
              {id, 1, "a"},
              {eq, 1, "="},
              {strlit, 1, "blub"},
              {rtag, 1, ">"},
              {semicolon, 1, ";"},
              {id, 2, "x"},
              {semicolon, 2, ";"}],
  R = r_rcd( [r_bind( a, r_var( x, t_str() ) )] ),
  E = rcd( 1, [e_bind( a, str( 1, <<"blub">> ) )] ),
  ?assertEqual( {ok, {[], [assign( 1, R, E )], [var( 2, x )]}}, parse( TokenLst ) ).

record_pattern_multi_match() ->
  TokenLst = [{assign, 1, "let"},
              {ltag, 1, "<"},
              {id, 1, "a"},
              {eq, 1, "="},
              {id, 1, "x"},
              {colon, 1, ":"},
              {t_bool, 1, "Bool"},
              {comma, 1, ","},
              {id, 1, "b"},
              {eq, 1, "="},
              {id, 1, "y"},
              {colon, 1, ":"},
              {lsquarebr, 1, "["},
              {t_file, 1, "File"},
              {rsquarebr, 1, "]"},
              {rtag, 1, ">"},
              {eq, 1, "="},
              {id, 1, "z"},
              {semicolon, 1, ";"},
              {id, 2, "y"},
              {semicolon, 2, ";"}],
  R = r_rcd( [r_bind( a, r_var( x, t_bool() ) ),
              r_bind( b, r_var( y, t_lst( t_file() ) ) )] ),
  E = var( 1, z ),
  ?assertEqual( {ok, {[], [assign( 1, R, E )], [var( 2, y )]}}, parse( TokenLst ) ).

record_field_access() ->
  TokenLst = [{assign, 1, "let"},
              {id, 1, "x"},
              {colon, 1, ":"},
              {ltag, 1, "<"},
              {id, 1, "a"},
              {colon, 1, ":"},
              {t_str, 1, "Str"},
              {rtag, 1, ">"},
              {eq, 1, "="},
              {ltag, 1, "<"},
              {id, 1, "a"},
              {eq, 1, "="},
              {strlit, 1, "blub"},
              {rtag, 1, ">"},
              {semicolon, 1, ";"},
              {lparen, 1, "("},
              {id, 2, "x"},
              {bar, 2, "|"},
              {id, 2, "a"},
              {rparen, 2, ")"},
              {semicolon, 2, ";"}],
  R = r_var( x, t_rcd( [t_arg( a, t_str() )] ) ),
  E1 = rcd( 1, [e_bind( a, str( 1, <<"blub">> ) )] ),
  E2 = proj( 2, a, var( 2, x ) ),
  ?assertEqual( {ok, {[], [assign( 1, R, E1 )], [E2]}}, parse( TokenLst ) ).

empty_list() ->
  TokenLst = [{lsquarebr, 1, "["},
              {colon, 1, ":"},
              {t_str, 1, "Str"},
              {rsquarebr, 1, "]"},
               {semicolon, 1, ";"}],
  E = lst( 1, t_str(), [] ),
  ?assertEqual( {ok, {[], [], [E]}}, parse( TokenLst ) ).

isnil_list() ->
  TokenLst = [{assign, 1, "let"},
              {id, 1, "l"},
              {colon, 1, ":"},
              {lsquarebr, 1, "["},
              {t_str, 1, "Str"},
              {rsquarebr, 1, "]"},
              {eq, 1, "="},
              {lsquarebr, 2, "["},
              {strlit, 2, "bla"},
              {comma, 2, ","},
              {strlit, 2, "blub"},
              {colon, 2, ":"},
              {t_str, 2, "Str"},
              {rsquarebr, 2, "]"},
              {semicolon, 2, ";"},
              {isnil, 3, "isnil"},
              {id, 3, "l"},
              {semicolon, 3, ";"}],
  R = r_var( l, t_lst( t_str() ) ),
  E1 = lst( 2, t_str(), [str( 2, <<"bla">> ), str( 2, <<"blub">> )] ),
  E2 = isnil( 3, var( 3, l ) ),
  ?assertEqual( {ok, {[], [assign( 1, R, E1 )], [E2]}}, parse( TokenLst ) ).

list_append() ->
  TokenLst = [{lparen, 1, "("},
              {lsquarebr, 1, "["},
              {strlit, 1, "bla"},
              {colon, 1, ":"},
              {t_str, 1, "Str"},
              {rsquarebr, 1, "]"},
              {plus, 1, "+"},
              {lsquarebr, 1, "["},
              {strlit, 1, "blub"},
              {colon, 1, ":"},
              {t_str, 1, "Str"},
              {rsquarebr, 1, "]"},
              {rparen, 1, ")"},
              {semicolon, 1, ";"}],
  E = append( 1, lst( 1, t_str(), [str( 1, <<"bla">> )] ),
                 lst( 1, t_str(), [str( 1, <<"blub">> )] ) ),
  ?assertEqual( {ok, {[], [], [E]}}, parse( TokenLst ) ).

const_native_function_let() ->
  TokenLst = [{def, 1, "def"},
              {id, 1, "f"},
              {lparen, 1, "("},
              {rparen, 1, ")"},
              {rarrow, 1, "->"},
              {t_str, 1, "Str"},
              {lbrace, 1, "{"},
              {assign, 2, "let"},
              {id, 2, "x"},
              {colon, 2, ":"},
              {t_str, 2, "Str"},
              {eq, 2, "="},
              {strlit, 2, "blub"},
              {semicolon, 2, ";"},
              {id, 3, "x"},
              {rbrace, 4, "}"},
              {id, 6, "f"},
              {lparen, 6, "("},
              {rparen, 6, ")"},
              {semicolon, 6, ";"}],
  TFn = t_fn( ntv, [], t_str() ),
  R = r_var( f, TFn ),
  EBody = app( 2,
               lam_ntv( 2, [lam_ntv_arg( x, t_str() )], var( 3, x ) ),
               [e_bind( x, str( 2, <<"blub">> ) )] ),
  Lam = fix( 1, lam_ntv( 1, [lam_ntv_arg( f, TFn )], EBody ) ),
  Q = app( 6, var( 6, f ), [] ),
  ?assertEqual( {ok, {[], [assign( 1, R, Lam )], [Q]}}, parse( TokenLst ) ).

arg_native_function_let() ->
  TokenLst = [{def, 1, "def"},
              {id, 1, "f"},
              {lparen, 1, "("},
              {id, 1, "a"},
              {colon, 1, ":"},
              {t_file, 1, "File"},
              {rparen, 1, ")"},
              {rarrow, 1, "->"},
              {t_str, 1, "Str"},
              {lbrace, 1, "{"},
              {assign, 2, "let"},
              {id, 2, "x"},
              {colon, 2, ":"},
              {t_str, 2, "Str"},
              {eq, 2, "="},
              {strlit, 2, "blub"},
              {semicolon, 2, ";"},
              {id, 3, "x"},
              {rbrace, 4, "}"},
              {id, 6, "f"},
              {lparen, 6, "("},
              {rparen, 6, ")"},
              {semicolon, 6, ";"}],
  TFn = t_fn( ntv, [t_arg( a, t_file() )], t_str() ),
  R = r_var( f, TFn ),
  EBody = app( 2,
               lam_ntv( 2, [lam_ntv_arg( x, t_str() )], var( 3, x ) ),
               [e_bind( x, str( 2, <<"blub">> ) )] ),
  Lam = fix( 1, lam_ntv( 1, [lam_ntv_arg( f, TFn ), lam_ntv_arg( a, t_file() )], EBody ) ),
  Q = app( 6, var( 6, f ), [] ),
  ?assertEqual( {ok, {[], [assign( 1, R, Lam )], [Q]}}, parse( TokenLst ) ).

latest_let_binds_closest() ->
  TokenLst = [{def, 1, "def"},
              {id, 1, "f"},
              {lparen, 1, "("},
              {rparen, 1, ")"},
              {rarrow, 1, "->"},
              {t_str, 1, "Str"},
              {lbrace, 1, "{"},
              {assign, 2, "let"},
              {id, 2, "x"},
              {colon, 2, ":"},
              {t_str, 2, "Str"},
              {eq, 2, "="},
              {strlit, 2, "blub"},
              {semicolon, 2, ";"},
              {assign, 3, "let"},
              {id, 3, "y"},
              {colon, 3, ":"},
              {t_file, 3, "File"},
              {eq, 3, "="},
              {filelit, 3, "bla.txt"},
              {semicolon, 3, ";"},
              {id, 4, "x"},
              {rbrace, 5, "}"},
              {id, 7, "f"},
              {lparen, 7, "("},
              {rparen, 7, ")"},
              {semicolon, 7, ";"}],
  TFn = t_fn( ntv, [], t_str() ),
  R = r_var( f, TFn ),
  EBody = app( 2,
               lam_ntv( 2,
                        [lam_ntv_arg( x, t_str() )],
                        app( 3,
                             lam_ntv( 3,
                                      [lam_ntv_arg( y, t_file() )],
                                      var( 4, x ) ),
                             [e_bind( y, file( 3, <<"bla.txt">> ) )] ) ),
               [e_bind( x, str( 2, <<"blub">> ) )] ),
  Lam = fix( 1, lam_ntv( 1, [lam_ntv_arg( f, TFn )], EBody ) ),
  Q = app( 7, var( 7, f ), [] ),
  ?assertEqual( {ok, {[], [assign( 1, R, Lam )], [Q]}}, parse( TokenLst ) ).

map() ->
  TokenLst = [{for, 1, "for"},
              {id, 1, "x"},
              {larrow, 1, "<-"},
              {id, 1, "x_lst"},
              {do, 1, "do"},
              {id, 2, "f"},
              {lparen, 2, "("},
              {id, 2, "x"},
              {eq, 2, "="},
              {id, 2, "x"},
              {rparen, 2, ")"},
              {colon, 2, ":"},
              {t_str, 2, "Str"},
              {halt, 3, "end"},
              {semicolon, 3, ";"}],
  EBody = app( 2, var( 2, f ), [e_bind( x, var( 2, x ) )] ),
  E = for( 1, t_str(), [e_bind( x, var( 1, x_lst ) )], EBody ),
  ?assertEqual( {ok, {[], [], [E]}}, parse( TokenLst ) ).

map_let() ->
  TokenLst = [{for, 1, "for"},
              {id, 1, "x"},
              {larrow, 1, "<-"},
              {id, 1, "x_lst"},
              {do, 1, "do"},
              {assign, 2, "let"},
              {id, 2, "y"},
              {colon, 2, ":"},
              {t_str, 2, "Str"},
              {eq, 2, "="},
              {id, 2, "f"},
              {lparen, 2, "("},
              {id, 2, "x"},
              {eq, 2, "="},
              {id, 2, "x"},
              {rparen, 2, ")"},
              {semicolon, 2, ";"},
              {id, 3, "y"},
              {colon, 3, ":"},
              {t_str, 3, "Str"},
              {halt, 4, "end"},
              {semicolon, 4, ";"}],
  EBody = app( 2, lam_ntv( 2, [lam_ntv_arg( y, t_str() )], var( 3, y ) ),
               [e_bind( y, app( 2, 
                                var( 2, f ),
                                [e_bind( x, var( 2, x ) )] ) )] ),
  E = for( 1, t_str(), [e_bind( x, var( 1, x_lst ) )], EBody ),
  ?assertEqual( {ok, {[], [], [E]}}, parse( TokenLst ) ).

zip_pair() ->
  TokenLst = [{for, 1, "for"},
              {id, 1, "x"},
              {larrow, 1, "<-"},
              {id, 1, "x_lst"},
              {comma, 1, ","},
              {id, 1, "y"},
              {larrow, 1, "<-"},
              {id, 1, "y_lst"},
              {do, 1, "do"},
              {id, 2, "f"},
              {lparen, 2, "("},
              {id, 2, "x"},
              {eq, 2, "="},
              {id, 2, "x"},
              {comma, 2, ","},
              {id, 2, "y"},
              {eq, 2, "="},
              {id, 2, "y"},
              {rparen, 2, ")"},
              {colon, 2, ":"},
              {t_str, 2, "Str"},
              {halt, 3, "end"},
              {semicolon, 3, ";"}],
  EBody = app( 2, var( 2, f ), [e_bind( x, var( 2, x ) ), e_bind( y, var( 2, y ) )] ),
  E = for( 1, t_str(), [e_bind( x, var( 1, x_lst ) ), e_bind( y, var( 1, y_lst ) )], EBody ),
  ?assertEqual( {ok, {[], [], [E]}}, parse( TokenLst ) ).

zip_triple() ->
  TokenLst = [{for, 1, "for"},
              {id, 1, "x"},
              {larrow, 1, "<-"},
              {id, 1, "x_lst"},
              {comma, 1, ","},
              {id, 1, "y"},
              {larrow, 1, "<-"},
              {id, 1, "y_lst"},
              {comma, 1, ","},
              {id, 1, "z"},
              {larrow, 1, "<-"},
              {id, 1, "z_lst"},
              {do, 1, "do"},
              {id, 2, "f"},
              {lparen, 2, "("},
              {id, 2, "x"},
              {eq, 2, "="},
              {id, 2, "x"},
              {comma, 2, ","},
              {id, 2, "y"},
              {eq, 2, "="},
              {id, 2, "y"},
              {comma, 2, ","},
              {id, 2, "z"},
              {eq, 2, "="},
              {id, 2, "z"},
              {rparen, 2, ")"},
              {colon, 2, ":"},
              {t_str, 2, "Str"},
              {halt, 3, "end"},
              {semicolon, 3, ";"}],
  EBody = app( 2, var( 2, f ), [e_bind( x, var( 2, x ) ), e_bind( y, var( 2, y ) ), e_bind( z, var( 2, z ) )] ),
  E = for( 1, t_str(), [e_bind( x, var( 1, x_lst ) ), e_bind( y, var( 1, y_lst ) ), e_bind( z, var( 1, z_lst ) )], EBody ),
  ?assertEqual( {ok, {[], [], [E]}}, parse( TokenLst ) ).

fold() ->
  TokenLst = [{fold, 1, "fold"},
              {id, 1, "x_acc"},
              {eq, 1, "="},
              {strlit, 1, "0"},
              {comma, 1, ","},
              {id, 1, "x"},
              {larrow, 1, "<-"},
              {id, 1, "x_lst"},
              {do, 1, "do"},
              {id, 2, "f"},
              {lparen, 2, "("},
              {id, 2, "x"},
              {eq, 2, "="},
              {id, 2, "x"},
              {comma, 2, ","},
              {id, 2, "x_acc"},
              {eq, 2, "="},
              {id, 2, "x_acc"},
              {rparen, 2, ")"},
              {halt, 3, "end"},
              {semicolon, 3, ";"}],
  EBody = app( 2, var( 2, f), [e_bind( x, var( 2, x ) ), e_bind( x_acc, var( 2, x_acc ) )] ),
  E = fold( 1, e_bind( x_acc, str( 1, <<"0">> ) ), e_bind( x, var( 1, x_lst ) ), EBody ),
  ?assertEqual( {ok, {[], [], [E]}}, parse( TokenLst ) ).

fold_let() ->
  TokenLst = [{fold, 1, "fold"},
              {id, 1, "x_acc"},
              {eq, 1, "="},
              {strlit, 1, "0"},
              {comma, 1, ","},
              {id, 1, "x"},
              {larrow, 1, "<-"},
              {id, 1, "x_lst"},
              {do, 1, "do"},
              {assign, 2, "let"},
              {id, 2, "y"},
              {colon, 2, ":"},
              {t_str, 2, "Str"},
              {eq, 2, "="},
              {id, 2, "f"},
              {lparen, 2, "("},
              {id, 2, "x"},
              {eq, 2, "="},
              {id, 2, "x"},
              {comma, 2, ","},
              {id, 2, "x_acc"},
              {eq, 2, "="},
              {id, 2, "x_acc"},
              {rparen, 2, ")"},
              {semicolon, 2, ";"},
              {id, 3, "y"},
              {halt, 4, "end"},
              {semicolon, 4, ";"}],
  EBody = app( 2,
               lam_ntv( 2, [lam_ntv_arg( y, t_str() )], var( 3, y ) ),
               [e_bind( y, app( 2,
                                var( 2, f),
                                [e_bind( x, var( 2, x ) ),
                                 e_bind( x_acc, var( 2, x_acc ) )] ) )] ),
  E = fold( 1, e_bind( x_acc, str( 1, <<"0">> ) ), e_bind( x, var( 1, x_lst ) ), EBody ),
  ?assertEqual( {ok, {[], [], [E]}}, parse( TokenLst ) ).