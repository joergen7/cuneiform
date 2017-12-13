-module( cuneiform_scan_test ).

-include_lib( "eunit/include/eunit.hrl" ).

-import( cuneiform_scan, [string/1] ).

%%====================================================================
%% Scanner
%%====================================================================

scan_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"string literal",       fun string_literal/0},
    {"file literal",         fun file_literal/0},
    {"true",                 fun true/0},
    {"false",                fun false/0},
    {"string comparison",    fun string_comparison/0},
    {"conditional",          fun conditional/0},
    {"negation",             fun negation/0},
    {"conjunction",          fun conjunction/0},
    {"disjunction",          fun disjunction/0},
    {"variable",             fun variable/0},
    {"function application", fun function_application/0},
    {"list literal",         fun list_literal/0},
    {"list append",          fun list_append/0},
    {"nil check",            fun nil_check/0},
    {"zip",                  fun zip/0},
    {"nested map",           fun nested_map/0},
    {"fold",                 fun fold/0},
    {"record literal",       fun record_literal/0},
    {"record field access",  fun record_field_access/0},
    {"let variable pattern", fun let_variable_pattern/0},
    {"let record pattern",   fun let_record_pattern/0},
    {"native function def",  fun native_function_def/0},
    {"foreign function def", fun foreign_function_def/0},
    {"import",               fun import/0}
   ]
  }.

string_literal() ->
  S = "\"blub\"",
  {ok, TokenLst, _} = string( S ),
  ?assertEqual( [{strlit, 1, "blub"}], TokenLst ).

file_literal() ->
  S = "'x.txt'",
  {ok, TokenLst, _} = string( S ),
  ?assertEqual( [{filelit, 1, "x.txt"}], TokenLst ).

true() ->
  S = "true",
  {ok, TokenLst, _} = string( S ),
  ?assertEqual( [{true, 1, "true"}], TokenLst ).

false() ->
  S = "false",
  {ok, TokenLst, _} = string( S ),
  ?assertEqual( [{false, 1, "false"}], TokenLst ).

string_comparison() ->
  S = "(\"a\"==\"b\")",
  {ok, TokenLst, _} = string( S ),
  ?assertEqual( [{lparen, 1, "("},
                 {strlit, 1, "a"},
                 {cmp, 1, "=="},
                 {strlit, 1, "b"},
                 {rparen, 1, ")"}], TokenLst ).

conditional() ->
  S = "if true then \"bla\" else \"blub\"",
  {ok, TokenLst, _} = string( S ),
  ?assertEqual( [{cnd, 1, "if"},
                 {true, 1, "true"},
                 {then, 1, "then"},
                 {strlit, 1, "bla"},
                 {else, 1, "else"},
                 {strlit, 1, "blub"}], TokenLst ).

negation() ->
  S = "not false",
  {ok, TokenLst, _} = string( S ),
  ?assertEqual( [{neg, 1, "not"},
                 {false, 1, "false"}], TokenLst ).

conjunction() ->
  S = "(true and false)",
  {ok, TokenLst, _} = string( S ),
  ?assertEqual( [{lparen, 1, "("},
                 {true, 1, "true"},
                 {wedge, 1, "and"},
                 {false, 1, "false"},
                 {rparen, 1, ")"}], TokenLst ).

disjunction() ->
  S = "(true or false)",
  {ok, TokenLst, _} = string( S ),
  ?assertEqual( [{lparen, 1, "("},
                 {true, 1, "true"},
                 {vee, 1, "or"},
                 {false, 1, "false"},
                 {rparen, 1, ")"}], TokenLst ).

variable() ->
  S = "x",
  {ok, TokenLst, _} = string( S ),
  ?assertEqual( [{id, 1, "x"}], TokenLst ).

function_application() ->
  S = "f( a = \"bla\", b = 'blub.txt' )",
  {ok, TokenLst, _} = string( S ),
  ?assertEqual( [{id, 1, "f"},
                 {lparen, 1, "("},
                 {id, 1, "a"},
                 {eq, 1, "="},
                 {strlit, 1, "bla"},
                 {comma, 1, ","},
                 {id, 1, "b"},
                 {eq, 1, "="},
                 {filelit, 1, "blub.txt"},
                 {rparen, 1, ")"}], TokenLst ).

list_literal() ->
  S = "[\"bla\", \"blub\" : Str]",
  {ok, TokenLst, _} = string( S ),
  ?assertEqual( [{lsquarebr, 1, "["},
                 {strlit, 1, "bla"},
                 {comma, 1, ","},
                 {strlit, 1, "blub"},
                 {colon, 1, ":"},
                 {t_str, 1, "Str"},
                 {rsquarebr, 1, "]"}], TokenLst ).

list_append() ->
  S = "([\"a\", \"b\" : Str] + [\"c\", \"d\" : Str])",
  {ok, TokenLst, _} = string( S ),
  ?assertEqual( [{lparen, 1, "("},
                 {lsquarebr, 1, "["},
                 {strlit, 1, "a"},
                 {comma, 1, ","},
                 {strlit, 1, "b"},
                 {colon, 1, ":"},
                 {t_str, 1, "Str"},
                 {rsquarebr, 1, "]"},
                 {plus, 1, "+"},
                 {lsquarebr, 1, "["},
                 {strlit, 1, "c"},
                 {comma, 1, ","},
                 {strlit, 1, "d"},
                 {colon, 1, ":"},
                 {t_str, 1, "Str"},
                 {rsquarebr, 1, "]"},
                 {rparen, 1, ")"}], TokenLst ).

nil_check() ->
  S = "isnil [: Str]",
  {ok, TokenLst, _} = string( S ),
  ?assertEqual( [{isnil, 1, "isnil"},
                 {lsquarebr, 1, "["},
                 {colon, 1, ":"},
                 {t_str, 1, "Str"},
                 {rsquarebr, 1, "]"}], TokenLst ).

zip() ->
  S = "for x <- l1, y <- l2 do <a = x, b = y>",
  {ok, TokenLst, _} = string( S ),
  ?assertEqual( [{for, 1, "for"},
                 {id, 1, "x"},
                 {larrow, 1, "<-"},
                 {id, 1, "l1"},
                 {comma, 1, ","},
                 {id, 1, "y"},
                 {larrow, 1, "<-"},
                 {id, 1, "l2"},
                 {do, 1, "do"},
                 {ltag, 1, "<"},
                 {id, 1, "a"},
                 {eq, 1, "="},
                 {id, 1, "x"},
                 {comma, 1, ","},
                 {id, 1, "b"},
                 {eq, 1, "="},
                 {id, 1, "y"},
                 {rtag, 1, ">"}], TokenLst ).

nested_map() ->
  S = "for x <- l1 do for y <- l2 do <a = x, b = y>",
  {ok, TokenLst, _} = string( S ),
  ?assertEqual( [{for, 1, "for"},
                 {id, 1, "x"},
                 {larrow, 1, "<-"},
                 {id, 1, "l1"},
                 {do, 1, "do"},
                 {for, 1, "for"},
                 {id, 1, "y"},
                 {larrow, 1, "<-"},
                 {id, 1, "l2"},
                 {do, 1, "do"},
                 {ltag, 1, "<"},
                 {id, 1, "a"},
                 {eq, 1, "="},
                 {id, 1, "x"},
                 {comma, 1, ","},
                 {id, 1, "b"},
                 {eq, 1, "="},
                 {id, 1, "y"},
                 {rtag, 1, ">"}], TokenLst ).

fold() ->
  S = "fold acc = x0, x <- l1 do f( a=x0, b=x )",
  {ok, TokenLst, _} = string( S ),
  ?assertEqual( [{fold, 1, "fold"},
                 {id, 1, "acc"},
                 {eq, 1, "="},
                 {id, 1, "x0"},
                 {comma, 1, ","},
                 {id, 1, "x"},
                 {larrow, 1, "<-"},
                 {id, 1, "l1"},
                 {do, 1, "do"},
                 {id, 1, "f"},
                 {lparen, 1, "("},
                 {id, 1, "a"},
                 {eq, 1, "="},
                 {id, 1, "x0"},
                 {comma, 1, ","},
                 {id, 1, "b"},
                 {eq, 1, "="},
                 {id, 1, "x"},
                 {rparen, 1, ")"}], TokenLst ).

record_literal() ->
  S = "<a = \"bla\", b = \"blub\">",
  {ok, TokenLst, _} = string( S ),
  ?assertEqual( [{ltag, 1, "<"},
                 {id, 1, "a"},
                 {eq, 1, "="},
                 {strlit, 1, "bla"},
                 {comma, 1, ","},
                 {id, 1, "b"},
                 {eq, 1, "="},
                 {strlit, 1, "blub"},
                 {rtag, 1, ">"}], TokenLst ).

record_field_access() ->
  S = "x|a",
  {ok, TokenLst, _} = string( S ),
  ?assertEqual( [{id, 1, "x"},
                 {bar, 1, "|"},
                 {id, 1, "a"}], TokenLst ).

let_variable_pattern() ->
  S = "x : Str = \"blub\"; x",
  {ok, TokenLst, _} = string( S ),
  ?assertEqual( [{id, 1, "x"},
                 {colon, 1, ":"},
                 {t_str, 1, "Str"},
                 {eq, 1, "="},
                 {strlit, 1, "blub"},
                 {semicolon, 1, ";"},
                 {id, 1, "x"}], TokenLst ).

let_record_pattern() ->
  S = "<a=x:Str, b=y:File> = <a=\"bla\", b='b.txt'>; x",
  {ok, TokenLst, _} = string( S ),
  ?assertEqual( [{ltag, 1, "<"},
                 {id, 1, "a"},
                 {eq, 1, "="},
                 {id, 1, "x"},
                 {colon, 1, ":"},
                 {t_str, 1, "Str"},
                 {comma, 1, ","},
                 {id, 1, "b"},
                 {eq, 1, "="},
                 {id, 1, "y"},
                 {colon, 1, ":"},
                 {t_file, 1, "File"},
                 {rtag, 1, ">"},
                 {eq, 1, "="},
                 {ltag, 1, "<"},
                 {id, 1, "a"},
                 {eq, 1, "="},
                 {strlit, 1, "bla"},
                 {comma, 1, ","},
                 {id, 1, "b"},
                 {eq, 1, "="},
                 {filelit, 1, "b.txt"},
                 {rtag, 1, ">"},
                 {semicolon, 1, ";"},
                 {id, 1, "x"}], TokenLst ).

native_function_def() ->
  S = "def f ( a : Str, b : File ) -> <c:Str, d:File> {\n"++
      "  c : Str  = a;\n  d : File = b;\n  <c = a, d = b>\n}\n\n"++
      "f( a = \"bla\", b = 'blub.txt' )",
  {ok, TokenLst, _} = string( S ),
  ?assertEqual( [{def, 1, "def"},
                 {id, 1, "f"},
                 {lparen, 1, "("},
                 {id, 1, "a"},
                 {colon, 1, ":"},
                 {t_str, 1, "Str"},
                 {comma, 1, ","},
                 {id, 1, "b"},
                 {colon, 1, ":"},
                 {t_file, 1, "File"},
                 {rparen, 1, ")"},
                 {rarrow, 1, "->"},
                 {ltag, 1, "<"},
                 {id, 1, "c"},
                 {colon, 1, ":"},
                 {t_str, 1, "Str"},
                 {comma, 1, ","},
                 {id, 1, "d"},
                 {colon, 1, ":"},
                 {t_file, 1, "File"},
                 {rtag, 1, ">"},
                 {lbrace, 1, "{"},
                 {id, 2, "c"},
                 {colon, 2, ":"},
                 {t_str, 2, "Str"},
                 {eq, 2, "="},
                 {id, 2, "a"},
                 {semicolon, 2, ";"},
                 {id, 3, "d"},
                 {colon, 3, ":"},
                 {t_file, 3, "File"},
                 {eq, 3, "="},
                 {id, 3, "b"},
                 {semicolon, 3, ";"},
                 {ltag, 4, "<"},
                 {id, 4, "c"},
                 {eq, 4, "="},
                 {id, 4, "a"},
                 {comma, 4, ","},
                 {id, 4, "d"},
                 {eq, 4, "="},
                 {id, 4, "b"},
                 {rtag, 4, ">"},
                 {rbrace, 5, "}"},
                 {id, 7, "f"},
                 {lparen, 7, "("},
                 {id, 7, "a"},
                 {eq, 7, "="},
                 {strlit, 7, "bla"},
                 {comma, 7, ","},
                 {id, 7, "b"},
                 {eq, 7, "="},
                 {filelit, 7, "blub.txt"},
                 {rparen, 7, ")"}], TokenLst ).

foreign_function_def() ->
  S = "def f ( a:Str, b:File ) -> <c:Str, d:File> in Bash *{\n"++
      "  c=$a\n  d=$b\n}*\n\nf( a=\"bla\", b='blub.txt' )",
  {ok, TokenLst, _} = string( S ),
  ?assertEqual( [{def, 1, "def"},
                 {id, 1, "f"},
                 {lparen, 1, "("},
                 {id, 1, "a"},
                 {colon, 1, ":"},
                 {t_str, 1, "Str"},
                 {comma, 1, ","},
                 {id, 1, "b"},
                 {colon, 1, ":"},
                 {t_file, 1, "File"},
                 {rparen, 1, ")"},
                 {rarrow, 1, "->"},
                 {ltag, 1, "<"},
                 {id, 1, "c"},
                 {colon, 1, ":"},
                 {t_str, 1, "Str"},
                 {comma, 1, ","},
                 {id, 1, "d"},
                 {colon, 1, ":"},
                 {t_file, 1, "File"},
                 {rtag, 1, ">"},
                 {in, 1, "in"},
                 {l_bash, 1, "Bash"},
                 {body, 1, "\n  c=$a\n  d=$b\n"},
                 {id, 6, "f"},
                 {lparen, 6, "("},
                 {id, 6, "a"},
                 {eq, 6, "="},
                 {strlit, 6, "bla"},
                 {comma, 6, ","},
                 {id, 6, "b"},
                 {eq, 6, "="},
                 {filelit, 6, "blub.txt"},
                 {rparen, 6, ")"}], TokenLst ).

import() ->
  S = "import 'blub.txt'; x",
  {ok, TokenLst, _} = string( S ),
  ?assertEqual( [{import, 1, "import"},
                 {filelit, 1, "blub.txt"},
                 {semicolon, 1, ";"},
                 {id, 1, "x"}], TokenLst ).