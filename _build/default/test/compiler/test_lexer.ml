(* tests/unit/test_lexer.ml
   OUnit2 unit tests for lexer.ml.
   Every function that can be unit-tested has at least 3 tests.
   Run: ocamlfind ocamlopt -package ounit2 -linkpkg
                  compiler/lexer/lexer.ml tests/unit/test_lexer.ml
                  -o run_tests && ./run_tests               *)

open OUnit2
open Gamedsl_lib.Lexer
(* ─── helpers ─────────────────────────────────────────────── *)

(* tokenize a raw string and return only the token list,
   stripping positions *)
let toks src = List.map (fun lt -> lt.token) (tokenize src)

(* tokenize and return just the first token *)
let first src = List.hd (toks src)

(* ================================================================
   SECTION A — peek / peek2 / advance / next / is_eof
   ================================================================ *)

let test_peek_normal _ =
  let s = make_stream "abc" in
  assert_equal 'a' (peek s)

let test_peek_eof _ =
  let s = make_stream "" in
  assert_equal '\x00' (peek s)

let test_peek_does_not_consume _ =
  let s = make_stream "xy" in
  let _ = peek s in
  assert_equal 'x' (peek s)   (* still 'x', not consumed *)

let test_peek2_normal _ =
  let s = make_stream "abc" in
  assert_equal 'b' (peek2 s)

let test_peek2_only_one_char _ =
  let s = make_stream "a" in
  assert_equal '\x00' (peek2 s)

let test_peek2_eof _ =
  let s = make_stream "" in
  assert_equal '\x00' (peek2 s)

let test_advance_moves_pos _ =
  let s = make_stream "hi" in
  advance s;
  assert_equal 'i' (peek s)

let test_advance_updates_col _ =
  let s = make_stream "ab" in
  advance s;
  assert_equal 2 s.col

let test_advance_newline_resets_col _ =
  let s = make_stream "a\nb" in
  advance s; advance s;   (* consume 'a' then '\n' *)
  assert_equal 1 s.col

let test_next_returns_char _ =
  let s = make_stream "xyz" in
  assert_equal 'x' (next s)

let test_next_advances _ =
  let s = make_stream "xyz" in
  let _ = next s in
  assert_equal 'y' (peek s)

let test_is_eof_empty _ =
  let s = make_stream "" in
  assert_bool "empty is eof" (is_eof s)

let test_is_eof_not_empty _ =
  let s = make_stream "a" in
  assert_bool "non-empty is not eof" (not (is_eof s))

let test_is_eof_after_consuming _ =
  let s = make_stream "a" in
  advance s;
  assert_bool "eof after consuming only char" (is_eof s)

(* ================================================================
   SECTION B — skip (whitespace + comments)
   ================================================================ *)

let test_skip_spaces _ =
  let s = make_stream "   abc" in
  skip s;
  assert_equal 'a' (peek s)

let test_skip_newlines _ =
  let s = make_stream "\n\n\nX" in
  skip s;
  assert_equal 'X' (peek s)

let test_skip_line_comment _ =
  let s = make_stream "// comment\nabc" in
  skip s;
  assert_equal 'a' (peek s)

let test_skip_block_comment _ =
  let s = make_stream "/* comment */ abc" in
  skip s;
  assert_equal 'a' (peek s)

let test_skip_block_comment_multiline _ =
  let s = make_stream "/* line1\n   line2 */ X" in
  skip s;
  assert_equal 'X' (peek s)

let test_skip_unterminated_block_comment _ =
  let s = make_stream "/* not closed" in
  assert_raises
    (LexError ("Unterminated block comment", { line = 1; col = 1 }))
    (fun () -> skip s)

(* ================================================================
   SECTION C — lex_number
   ================================================================ *)

let test_lex_number_int _ =
  assert_equal (INT_LIT 42) (first "42")

let test_lex_number_zero _ =
  assert_equal (INT_LIT 0) (first "0")

let test_lex_number_large _ =
  assert_equal (INT_LIT 9999) (first "9999")

let test_lex_number_float _ =
  assert_equal (FLOAT_LIT 3.14) (first "3.14")

let test_lex_number_float_leading_zero _ =
  assert_equal (FLOAT_LIT 0.5) (first "0.5")

let test_lex_number_grid_dim _ =
  assert_equal (GRID_DIM_LIT (20, 15)) (first "20x15")

let test_lex_number_grid_dim_square _ =
  assert_equal (GRID_DIM_LIT (10, 10)) (first "10x10")

let test_lex_number_duration _ =
  assert_equal (DURATION_LIT 120) (first "120s")

let test_lex_number_duration_one _ =
  assert_equal (DURATION_LIT 1) (first "1s")

let test_lex_number_percent _ =
  assert_equal (PERCENT_LIT 30) (first "30%")

let test_lex_number_percent_100 _ =
  assert_equal (PERCENT_LIT 100) (first "100%")

let test_lex_number_int_not_duration_in_word _ =
  (* "1start" — 's' is followed by alnum so must stay INT *)
  let ts = toks "1start" in
  assert_equal (INT_LIT 1) (List.nth ts 0)

(* ================================================================
   SECTION D — lex_string
   ================================================================ *)

let test_lex_string_simple _ =
  assert_equal (STRING_LIT "Goblin") (first "\"Goblin\"")

let test_lex_string_with_underscore _ =
  assert_equal (STRING_LIT "P1_hero") (first "\"P1_hero\"")

let test_lex_string_empty _ =
  assert_equal (STRING_LIT "") (first "\"\"")

let test_lex_filepath_slash _ =
  assert_equal (FILEPATH_LIT "assets/goblin.png") (first "\"assets/goblin.png\"")

let test_lex_filepath_dot _ =
  assert_equal (FILEPATH_LIT "img.png") (first "\"img.png\"")

let test_lex_filepath_dash _ =
  assert_equal (FILEPATH_LIT "my-sprite.png") (first "\"my-sprite.png\"")

let test_lex_string_unterminated _ =
  assert_raises
    (LexError ("Unterminated string missing closing '\"'", { line = 1; col = 1 }))
    (fun () -> toks "\"no close")

let test_lex_string_newline_inside _ =
  assert_raises
    (LexError ("Unterminated string- newline inside quotes ", { line = 1; col = 1 }))
    (fun () -> toks "\"bad\nnewline\"")

(* ================================================================
   SECTION E — lex_ident / lookup / is_key
   ================================================================ *)

let test_lookup_keyword_world _ =
  assert_equal WORLD (first "world")

let test_lookup_keyword_player _ =
  assert_equal PLAYER (first "player")

let test_lookup_keyword_active _ =
  assert_equal ACTIVE (first "active")

let test_lookup_keyword_true _ =
  assert_equal TRUE (first "true")

let test_lookup_keyword_false _ =
  assert_equal FALSE (first "false")

let test_lookup_keyword_spawn _ =
  assert_equal SPAWN (first "spawn")

let test_lookup_keyword_infinite _ =
  assert_equal INFINITE (first "infinite")

let test_lookup_fallback_to_ident _ =
  assert_equal (IDENT "myVar") (first "myVar")

let test_lookup_ident_with_underscore _ =
  assert_equal (IDENT "my_var") (first "my_var")

let test_is_key_space _ =
  assert_equal (KEY_LIT "SPACE") (first "SPACE")

let test_is_key_auto _ =
  assert_equal (KEY_LIT "AUTO") (first "AUTO")

let test_is_key_w _ =
  assert_equal (KEY_LIT "W") (first "W")

let test_is_key_up _ =
  (* "UP" is both a KEY_LIT and a keyword — key_set takes precedence *)
  assert_equal (KEY_LIT "UP") (first "UP")

(* ================================================================
   SECTION F — lex_op
   ================================================================ *)

let test_op_lbrace _ =
  assert_equal LBRACE (first "{")

let test_op_rbrace _ =
  assert_equal RBRACE (first "}")

let test_op_eq _ =
  assert_equal EQ (first "==")

let test_op_assign _ =
  assert_equal ASSIGN_OP (first "=")

let test_op_neq _ =
  assert_equal NEQ (first "!=")

let test_op_geq _ =
  assert_equal GEQ (first ">=")

let test_op_leq _ =
  assert_equal LEQ (first "<=")

let test_op_gt _ =
  assert_equal GT (first ">")

let test_op_lt _ =
  assert_equal LT (first "<")

let test_op_unexpected_char _ =
  assert_raises
    (LexError ("Unexpected character '@' (ASCII 64)", { line = 1; col = 1 }))
    (fun () -> toks "@")

let test_op_unexpected_bang _ =
  assert_raises
    (LexError ("Unexpected '!' — did you mean '!='", { line = 1; col = 1 }))
    (fun () -> toks "! ")

(* ================================================================
   SECTION G — tokenize (full pipeline)
   ================================================================ *)

let test_tokenize_empty _ =
  assert_equal [ EOF ] (toks "")

let test_tokenize_eof_always_last _ =
  let ts = toks "42" in
  assert_equal EOF (List.nth ts (List.length ts - 1))

let test_tokenize_world_block _ =
  let ts = toks "world { grid_size: 20x15 duration: 120s }" in
  assert_equal WORLD       (List.nth ts 0);
  assert_equal LBRACE      (List.nth ts 1);
  assert_equal GRID_SIZE   (List.nth ts 2);
  assert_equal COLON       (List.nth ts 3);
  assert_equal (GRID_DIM_LIT (20,15)) (List.nth ts 4);
  assert_equal DURATION_KW (List.nth ts 5)

let test_tokenize_skips_comments _ =
  let ts = toks "// comment\nworld" in
  assert_equal WORLD (List.nth ts 0)

let test_tokenize_spawn_no_position _ =
  let ts = toks "spawn \"Goblin\"" in
  assert_equal SPAWN               (List.nth ts 0);
  assert_equal (STRING_LIT "Goblin") (List.nth ts 1)

let test_tokenize_spawn_with_position _ =
  let ts = toks "spawn \"P2\" position: 5, 5" in
  assert_equal SPAWN               (List.nth ts 0);
  assert_equal (STRING_LIT "P2")   (List.nth ts 1);
  assert_equal POSITION            (List.nth ts 2)

let test_tokenize_active_false _ =
  let ts = toks "active: false" in
  assert_equal ACTIVE  (List.nth ts 0);
  assert_equal COLON   (List.nth ts 1);
  assert_equal FALSE   (List.nth ts 2)

let test_tokenize_active_true _ =
  let ts = toks "active: true" in
  assert_equal TRUE (List.nth ts 2)

let test_tokenize_percent_literal _ =
  let ts = toks "damage_reduction: 30%" in
  assert_equal (PERCENT_LIT 30) (List.nth ts 2)

let test_tokenize_float_expr _ =
  let ts = toks "var x = 1.5 * 2.0" in
  assert_equal (FLOAT_LIT 1.5) (List.nth ts 3);
  assert_equal STAR            (List.nth ts 4);
  assert_equal (FLOAT_LIT 2.0) (List.nth ts 5)

(* ================================================================
   SECTION H — token_to_string
   ================================================================ *)

let test_to_string_world _ =
  assert_equal "WORLD" (token_to_string WORLD)

let test_to_string_int_lit _ =
  assert_equal "INT(42)" (token_to_string (INT_LIT 42))

let test_to_string_grid_dim _ =
  assert_equal "GRID_DIM(20x15)" (token_to_string (GRID_DIM_LIT (20,15)))

let test_to_string_duration _ =
  assert_equal "DURATION(120s)" (token_to_string (DURATION_LIT 120))

let test_to_string_percent _ =
  assert_equal "PERCENT_LIT(30%)" (token_to_string (PERCENT_LIT 30))

let test_to_string_true _ =
  assert_equal "TRUE" (token_to_string TRUE)

let test_to_string_false _ =
  assert_equal "FALSE" (token_to_string FALSE)

let test_to_string_spawn _ =
  assert_equal "SPAWN" (token_to_string SPAWN)

let test_to_string_ident _ =
  assert_equal "IDENT(myVar)" (token_to_string (IDENT "myVar"))

let test_to_string_filepath _ =
  assert_equal "FILEPATH(\"assets/x.png\")"
    (token_to_string (FILEPATH_LIT "assets/x.png"))

(* ================================================================
   SUITE ASSEMBLY
   Every test group is listed here.
   Adding a new test: write the function above, then add it here.
   ================================================================ *)

let stream_tests = "Stream operations" >::: [
  "peek normal"                >:: test_peek_normal;
  "peek eof"                   >:: test_peek_eof;
  "peek no consume"            >:: test_peek_does_not_consume;
  "peek2 normal"               >:: test_peek2_normal;
  "peek2 one char"             >:: test_peek2_only_one_char;
  "peek2 eof"                  >:: test_peek2_eof;
  "advance moves pos"          >:: test_advance_moves_pos;
  "advance updates col"        >:: test_advance_updates_col;
  "advance newline resets col" >:: test_advance_newline_resets_col;
  "next returns char"          >:: test_next_returns_char;
  "next advances"              >:: test_next_advances;
  "is_eof empty"               >:: test_is_eof_empty;
  "is_eof not empty"           >:: test_is_eof_not_empty;
  "is_eof after consuming"     >:: test_is_eof_after_consuming;
]

let skip_tests = "Skip whitespace and comments" >::: [
  "skip spaces"                      >:: test_skip_spaces;
  "skip newlines"                    >:: test_skip_newlines;
  "skip line comment"                >:: test_skip_line_comment;
  "skip block comment"               >:: test_skip_block_comment;
  "skip block comment multiline"     >:: test_skip_block_comment_multiline;
  "skip unterminated block comment"  >:: test_skip_unterminated_block_comment;
]

let number_tests = "lex_number" >::: [
  "int"                      >:: test_lex_number_int;
  "zero"                     >:: test_lex_number_zero;
  "large"                    >:: test_lex_number_large;
  "float"                    >:: test_lex_number_float;
  "float leading zero"       >:: test_lex_number_float_leading_zero;
  "grid dim"                 >:: test_lex_number_grid_dim;
  "grid dim square"          >:: test_lex_number_grid_dim_square;
  "duration"                 >:: test_lex_number_duration;
  "duration one"             >:: test_lex_number_duration_one;
  "percent"                  >:: test_lex_number_percent;
  "percent 100"              >:: test_lex_number_percent_100;
  "s not consumed in word"   >:: test_lex_number_int_not_duration_in_word;
]

let string_tests = "lex_string" >::: [
  "simple"          >:: test_lex_string_simple;
  "underscore"      >:: test_lex_string_with_underscore;
  "empty"           >:: test_lex_string_empty;
  "filepath slash"  >:: test_lex_filepath_slash;
  "filepath dot"    >:: test_lex_filepath_dot;
  "filepath dash"   >:: test_lex_filepath_dash;
  "unterminated"    >:: test_lex_string_unterminated;
  "newline inside"  >:: test_lex_string_newline_inside;
]

let ident_tests = "lex_ident / lookup / is_key" >::: [
  "keyword world"     >:: test_lookup_keyword_world;
  "keyword player"    >:: test_lookup_keyword_player;
  "keyword active"    >:: test_lookup_keyword_active;
  "keyword true"      >:: test_lookup_keyword_true;
  "keyword false"     >:: test_lookup_keyword_false;
  "keyword spawn"     >:: test_lookup_keyword_spawn;
  "keyword infinite"  >:: test_lookup_keyword_infinite;
  "fallback ident"    >:: test_lookup_fallback_to_ident;
  "ident underscore"  >:: test_lookup_ident_with_underscore;
  "key SPACE"         >:: test_is_key_space;
  "key AUTO"          >:: test_is_key_auto;
  "key W"             >:: test_is_key_w;
  "key UP"            >:: test_is_key_up;
]

let op_tests = "lex_op" >::: [
  "lbrace"          >:: test_op_lbrace;
  "rbrace"          >:: test_op_rbrace;
  "eq"              >:: test_op_eq;
  "assign"          >:: test_op_assign;
  "neq"             >:: test_op_neq;
  "geq"             >:: test_op_geq;
  "leq"             >:: test_op_leq;
  "gt"              >:: test_op_gt;
  "lt"              >:: test_op_lt;
  "unexpected char" >:: test_op_unexpected_char;
  "unexpected bang" >:: test_op_unexpected_bang;
]

let tokenize_tests = "tokenize pipeline" >::: [
  "empty"               >:: test_tokenize_empty;
  "eof always last"     >:: test_tokenize_eof_always_last;
  "world block"         >:: test_tokenize_world_block;
  "skips comments"      >:: test_tokenize_skips_comments;
  "spawn no position"   >:: test_tokenize_spawn_no_position;
  "spawn with position" >:: test_tokenize_spawn_with_position;
  "active false"        >:: test_tokenize_active_false;
  "active true"         >:: test_tokenize_active_true;
  "percent literal"     >:: test_tokenize_percent_literal;
  "float expr"          >:: test_tokenize_float_expr;
]

let to_string_tests = "token_to_string" >::: [
  "world"     >:: test_to_string_world;
  "int lit"   >:: test_to_string_int_lit;
  "grid dim"  >:: test_to_string_grid_dim;
  "duration"  >:: test_to_string_duration;
  "percent"   >:: test_to_string_percent;
  "true"      >:: test_to_string_true;
  "false"     >:: test_to_string_false;
  "spawn"     >:: test_to_string_spawn;
  "ident"     >:: test_to_string_ident;
  "filepath"  >:: test_to_string_filepath;
]

let () =
  run_test_tt_main ("GameDSL lexer" >::: [
    stream_tests;
    skip_tests;
    number_tests;
    string_tests;
    ident_tests;
    op_tests;
    tokenize_tests;
    to_string_tests;
  ])