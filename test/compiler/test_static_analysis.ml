(* tests/unit/test_semant.ml
   OUnit2 unit tests for semant.ml.
   Each check function in semant.ml has at least one test that
   triggers it (negative case) and at least one that confirms
   a clean program passes it (positive case).

   Run:
     ocamlfind ocamlopt -package ounit2 -linkpkg \
       compiler/lexer/lexer.ml ast.ml parser.ml semant.ml \
       tests/unit/test_semant.ml -o run_semant_tests
     ./run_semant_tests
*)

open OUnit2
open Gamedsl_lib.Ast
open Gamedsl_lib.Static_analysis

(* ============================================================
   HELPERS — build minimal AST fragments by hand so each test
   isolates exactly one rule, instead of parsing a full .gdsl
   file (which would couple these tests to the parser).
   ============================================================ *)

let default_world = { grid = { width = 20; height = 15 }; duration = 60 }
let default_controls ~up ~down ~left ~right = { up; down; left; right }

let mk_player ?(name = "P1") ?(active = true) ?(up = "W") ?(down = "S")
    ?(left = "A") ?(right = "D") () =
  {
    p_name = name;
    p_health = EInt 100;
    p_img = None;
    p_active = active;
    p_position = None;
    p_controls = default_controls ~up ~down ~left ~right;
  }

let mk_monster ?(name = "Goblin") ?(count = EInt 1) ?(movement = MvRandom) () =
  {
    m_name = name;
    m_health = EInt 50;
    m_img = None;
    m_movement = movement;
    m_count = count;
    m_position = None;
  }

let mk_ability ?(name = "Sword") ~atype ~body () =
  { a_name = name; a_type = atype; a_img = None; a_body = body }

let mk_win_survive () = { w_fields = [ WSurvive 60 ] }

let base_program ?(players = [ mk_player () ]) ?(monsters = [])
    ?(abilities = []) ?(assigns = []) ?(obstacles = []) ?(pre = []) ?(post = [])
    ?(win = mk_win_survive ()) () =
  {
    pre_stmts = pre;
    prog_world = default_world;
    players;
    abilities;
    monsters;
    assigns;
    obstacles;
    win_condition = win;
    post_stmts = post;
  }

let errors_of prog = (check_program prog).errors

let contains_substring needle haystack =
  let nlen = String.length needle and hlen = String.length haystack in
  let rec go i =
    i + nlen <= hlen && (String.sub haystack i nlen = needle || go (i + 1))
  in
  go 0

let assert_has_error_containing prog substr =
  let errs = errors_of prog in
  assert_bool
    (Printf.sprintf "expected an error containing %S, got: [%s]" substr
       (String.concat " | " errs))
    (List.exists (contains_substring substr) errs)

let assert_no_errors prog =
  let errs = errors_of prog in
  assert_equal ~printer:(String.concat " | ") [] errs

(* ============================================================
   SECTION A — global structure checks
   ============================================================ *)

let test_clean_minimal_program_has_no_errors _ =
  assert_no_errors (base_program ())

let test_no_win_fields_is_error _ =
  let prog = base_program ~win:{ w_fields = [] } () in
  assert_has_error_containing prog "at least one win field"

let test_zero_width_grid_is_error _ =
  let prog = base_program () in
  let prog =
    {
      prog with
      prog_world = { grid = { width = 0; height = 10 }; duration = 60 };
    }
  in
  assert_has_error_containing prog "positive width and height"

let test_zero_duration_is_error _ =
  let prog = base_program () in
  let prog =
    {
      prog with
      prog_world = { grid = { width = 10; height = 10 }; duration = 0 };
    }
  in
  assert_has_error_containing prog "positive number of seconds"

(* ============================================================
   SECTION B — symbol table / duplicate name checks
   ============================================================ *)

let test_duplicate_player_name_is_error _ =
  let prog =
    base_program
      ~players:
        [
          mk_player ~name:"P1" ~up:"W" ~down:"S" ~left:"A" ~right:"D" ();
          mk_player ~name:"P1" ~up:"UP" ~down:"DOWN" ~left:"LEFT" ~right:"RIGHT"
            ();
        ]
      ()
  in
  assert_has_error_containing prog "Duplicate player name"

let test_duplicate_monster_name_is_error _ =
  let prog =
    base_program
      ~monsters:[ mk_monster ~name:"Goblin" (); mk_monster ~name:"Goblin" () ]
      ()
  in
  assert_has_error_containing prog "Duplicate monster name"

let test_duplicate_ability_name_is_error _ =
  let ab1 =
    mk_ability ~name:"Sword" ~atype:Permanent
      ~body:[ AbField (FHealthRegen (EInt 5)) ]
      ()
  in
  let ab2 =
    mk_ability ~name:"Sword" ~atype:Permanent
      ~body:[ AbField (FSpeedBoost (EInt 1)) ]
      ()
  in
  let prog = base_program ~abilities:[ ab1; ab2 ] () in
  assert_has_error_containing prog "Duplicate ability name"

let test_unique_names_no_error _ =
  let prog =
    base_program
      ~monsters:[ mk_monster ~name:"Goblin" (); mk_monster ~name:"Troll" () ]
      ()
  in
  assert_no_errors prog

(* ============================================================
   SECTION C — key conflict checks
   ============================================================ *)

let test_two_players_same_key_is_error _ =
  let prog =
    base_program
      ~players:
        [
          mk_player ~name:"P1" ~up:"W" ~down:"S" ~left:"A" ~right:"D" ();
          mk_player ~name:"P2" ~up:"W" ~down:"DOWN" ~left:"LEFT" ~right:"RIGHT"
            ();
        ]
      ()
  in
  assert_has_error_containing prog "Key conflict"

let test_two_players_distinct_keys_no_error _ =
  let prog =
    base_program
      ~players:
        [
          mk_player ~name:"P1" ~up:"W" ~down:"S" ~left:"A" ~right:"D" ();
          mk_player ~name:"P2" ~up:"UP" ~down:"DOWN" ~left:"LEFT" ~right:"RIGHT"
            ();
        ]
      ()
  in
  assert_no_errors prog

let test_player_ability_with_auto_key_is_error _ =
  let ab =
    mk_ability ~name:"Bite" ~atype:Active
      ~body:
        [
          AbField (FKey "AUTO");
          AbField (FDamage (EInt 10));
          AbField (FRange (EInt 1));
          AbField (FShape Manhattan);
        ]
      ()
  in
  let prog =
    base_program
      ~players:[ mk_player ~name:"P1" () ]
      ~abilities:[ ab ]
      ~assigns:
        [ { asg_target = TgString "P1"; asg_abilities = [ AbName "Bite" ] } ]
      ()
  in
  assert_has_error_containing prog "forbidden on player abilities"

let test_monster_ability_without_auto_key_is_error _ =
  let ab =
    mk_ability ~name:"Bite" ~atype:Active
      ~body:
        [
          AbField (FKey "SPACE");
          AbField (FDamage (EInt 10));
          AbField (FRange (EInt 1));
          AbField (FShape Manhattan);
        ]
      ()
  in
  let prog =
    base_program
      ~monsters:[ mk_monster ~name:"Goblin" () ]
      ~abilities:[ ab ]
      ~assigns:
        [
          { asg_target = TgString "Goblin"; asg_abilities = [ AbName "Bite" ] };
        ]
      ()
  in
  assert_has_error_containing prog "must use AUTO"

(* ============================================================
   SECTION D — ability type field rules
   ============================================================ *)

let test_active_ability_missing_key_is_error _ =
  let ab =
    mk_ability ~atype:Active
      ~body:
        [
          AbField (FDamage (EInt 10));
          AbField (FRange (EInt 1));
          AbField (FShape Manhattan);
        ]
      ()
  in
  let prog = base_program ~abilities:[ ab ] () in
  assert_has_error_containing prog "missing required field: key"

let test_active_ability_complete_no_error _ =
  let ab =
    mk_ability ~atype:Active
      ~body:
        [
          AbField (FKey "SPACE");
          AbField (FDamage (EInt 10));
          AbField (FRange (EInt 1));
          AbField (FShape Manhattan);
        ]
      ()
  in
  let prog = base_program ~abilities:[ ab ] () in
  assert_no_errors prog

let test_permanent_ability_with_no_effect_field_is_error _ =
  let ab = mk_ability ~atype:Permanent ~body:[] () in
  let prog = base_program ~abilities:[ ab ] () in
  assert_has_error_containing prog "needs at least one of"

let test_permanent_ability_with_key_is_error _ =
  let ab =
    mk_ability ~atype:Permanent
      ~body:[ AbField (FKey "SPACE"); AbField (FHealthRegen (EInt 5)) ]
      ()
  in
  let prog = base_program ~abilities:[ ab ] () in
  assert_has_error_containing prog "must not have key"

let test_timed_ability_missing_activates_at_is_error _ =
  let ab =
    mk_ability ~atype:Timed
      ~body:
        [
          AbField (FKey "E");
          AbField (FDamage (EInt 10));
          AbField (FRange (EInt 1));
          AbField (FShape Manhattan);
        ]
      ()
  in
  let prog = base_program ~abilities:[ ab ] () in
  assert_has_error_containing prog "missing required field: activates_at"

let test_kill_unlocked_missing_required_kills_is_error _ =
  let ab =
    mk_ability ~atype:KillUnlocked
      ~body:[ AbField (FKey "Q"); AbField (FDamageMultiplier (EInt 2)) ]
      ()
  in
  let prog = base_program ~abilities:[ ab ] () in
  assert_has_error_containing prog "missing required field: required_kills"

let test_spread_without_directional_shape_is_error _ =
  let ab =
    mk_ability ~atype:Active
      ~body:
        [
          AbField (FKey "SPACE");
          AbField (FDamage (EInt 10));
          AbField (FRange (EInt 1));
          AbField (FShape Manhattan);
          AbField (FSpread (EInt 1));
        ]
      ()
  in
  let prog = base_program ~abilities:[ ab ] () in
  assert_has_error_containing prog "spread but shape is not directional"

(* ============================================================
   SECTION E — assign statement checks
   ============================================================ *)

let test_assign_to_undefined_target_is_error _ =
  let prog =
    base_program
      ~assigns:[ { asg_target = TgString "Ghost"; asg_abilities = [] } ]
      ()
  in
  assert_has_error_containing prog "is not a defined player or monster"

let test_assign_undefined_ability_is_error _ =
  let prog =
    base_program
      ~players:[ mk_player ~name:"P1" () ]
      ~assigns:
        [ { asg_target = TgString "P1"; asg_abilities = [ AbName "Nope" ] } ]
      ()
  in
  assert_has_error_containing prog "undefined ability"

let test_assign_duplicate_ability_in_same_call_is_error _ =
  let ab =
    mk_ability ~name:"Sword" ~atype:Permanent
      ~body:[ AbField (FHealthRegen (EInt 5)) ]
      ()
  in
  let prog =
    base_program
      ~players:[ mk_player ~name:"P1" () ]
      ~abilities:[ ab ]
      ~assigns:
        [
          {
            asg_target = TgString "P1";
            asg_abilities = [ AbName "Sword"; AbName "Sword" ];
          };
        ]
      ()
  in
  assert_has_error_containing prog "more than once"

let test_assign_valid_no_error _ =
  let ab =
    mk_ability ~name:"Shield" ~atype:Permanent
      ~body:[ AbField (FHealthRegen (EInt 5)) ]
      ()
  in
  let prog =
    base_program
      ~players:[ mk_player ~name:"P1" () ]
      ~abilities:[ ab ]
      ~assigns:
        [ { asg_target = TgString "P1"; asg_abilities = [ AbName "Shield" ] } ]
      ()
  in
  assert_no_errors prog

(* ============================================================
   SECTION F — monster checks
   ============================================================ *)

let test_monster_negative_count_is_error _ =
  let prog = base_program ~monsters:[ mk_monster ~count:(EInt (-1)) () ] () in
  assert_has_error_containing prog "count must be >= 0"

let test_monster_zero_count_no_error _ =
  let prog = base_program ~monsters:[ mk_monster ~count:(EInt 0) () ] () in
  assert_no_errors prog

let test_monster_towards_undefined_player_is_error _ =
  let prog =
    base_program ~monsters:[ mk_monster ~movement:(MvTowards "Ghost") () ] ()
  in
  assert_has_error_containing prog "is not a defined player"

let test_monster_towards_defined_player_no_error _ =
  let prog =
    base_program
      ~players:[ mk_player ~name:"P1" () ]
      ~monsters:[ mk_monster ~movement:(MvTowards "P1") () ]
      ()
  in
  assert_no_errors prog

(* ============================================================
   SECTION G — spawn statement checks
   ============================================================ *)

let test_spawn_undefined_entity_is_error _ =
  let prog =
    base_program
      ~post:[ SSpawn { spawn_name = "Ghost"; spawn_position = None } ]
      ()
  in
  assert_has_error_containing prog "not a defined player or monster"

let test_spawn_defined_monster_no_error _ =
  let prog =
    base_program
      ~monsters:[ mk_monster ~name:"Goblin" ~count:(EInt 0) () ]
      ~post:[ SSpawn { spawn_name = "Goblin"; spawn_position = None } ]
      ()
  in
  assert_no_errors prog

let test_spawn_inside_loop_undefined_is_error _ =
  let loop =
    {
      loop_times = LFinite 1;
      loop_actions = [ ASpawn { spawn_name = "Nope"; spawn_position = None } ];
    }
  in
  let prog = base_program ~post:[ SLoop loop ] () in
  assert_has_error_containing prog "not a defined player or monster"

(* ============================================================
   SECTION H — function checks
   ============================================================ *)

let test_fn_duplicate_param_is_error _ =
  let f = { fn_name = "f"; fn_params = [ "x"; "x" ]; fn_body = [] } in
  let prog = base_program ~pre:[ SFnDecl f ] () in
  assert_has_error_containing prog "duplicate parameter"

let test_fn_self_recursion_is_error _ =
  let f =
    {
      fn_name = "f";
      fn_params = [];
      fn_body = [ SFnCall { call_name = "f"; call_args = [] } ];
    }
  in
  let prog = base_program ~pre:[ SFnDecl f ] () in
  assert_has_error_containing prog "recursion is not allowed"

let test_fn_call_undefined_is_error _ =
  let prog =
    base_program ~post:[ SFnCall { call_name = "nope"; call_args = [] } ] ()
  in
  assert_has_error_containing prog "undefined function"

let test_fn_call_wrong_arity_is_error _ =
  let f = { fn_name = "boost"; fn_params = [ "name"; "amt" ]; fn_body = [] } in
  let prog =
    base_program ~pre:[ SFnDecl f ]
      ~post:[ SFnCall { call_name = "boost"; call_args = [ EInt 5 ] } ]
      ()
  in
  assert_has_error_containing prog "expects 2 argument(s) but call provides 1"

let test_fn_call_correct_arity_no_error _ =
  let f = { fn_name = "boost"; fn_params = [ "name"; "amt" ]; fn_body = [] } in
  let prog =
    base_program ~pre:[ SFnDecl f ]
      ~post:
        [ SFnCall { call_name = "boost"; call_args = [ EVar "x"; EInt 5 ] } ]
      ()
  in
  assert_no_errors prog

let test_fn_nested_decl_is_error _ =
  let inner = { fn_name = "inner"; fn_params = []; fn_body = [] } in
  let outer =
    { fn_name = "outer"; fn_params = []; fn_body = [ SFnDecl inner ] }
  in
  let prog = base_program ~pre:[ SFnDecl outer ] () in
  assert_has_error_containing prog "nested function declaration"

(* ============================================================
   SECTION I — entity reference checks (field_override)
   ============================================================ *)

let test_field_override_on_undefined_entity_is_error _ =
  let prog =
    base_program
      ~post:[ SFieldOverride (RefString "Ghost", [ "health" ], EInt 100) ]
      ()
  in
  assert_has_error_containing prog "not a defined player, monster, or ability"

let test_field_override_on_defined_monster_no_error _ =
  let prog =
    base_program
      ~monsters:[ mk_monster ~name:"Goblin" () ]
      ~post:[ SFieldOverride (RefString "Goblin", [ "health" ], EInt 100) ]
      ()
  in
  assert_no_errors prog

(* ============================================================
   SUITE ASSEMBLY
   ============================================================ *)

let global_structure_tests =
  "Global structure"
  >::: [
         "clean minimal program" >:: test_clean_minimal_program_has_no_errors;
         "no win fields" >:: test_no_win_fields_is_error;
         "zero width grid" >:: test_zero_width_grid_is_error;
         "zero duration" >:: test_zero_duration_is_error;
       ]

let symbol_table_tests =
  "Symbol tables / duplicates"
  >::: [
         "duplicate player" >:: test_duplicate_player_name_is_error;
         "duplicate monster" >:: test_duplicate_monster_name_is_error;
         "duplicate ability" >:: test_duplicate_ability_name_is_error;
         "unique names ok" >:: test_unique_names_no_error;
       ]

let key_conflict_tests =
  "Key conflicts"
  >::: [
         "two players same key" >:: test_two_players_same_key_is_error;
         "two players distinct keys" >:: test_two_players_distinct_keys_no_error;
         "player ability AUTO key"
         >:: test_player_ability_with_auto_key_is_error;
         "monster ability non-AUTO"
         >:: test_monster_ability_without_auto_key_is_error;
       ]

let ability_type_tests =
  "Ability type field rules"
  >::: [
         "active missing key" >:: test_active_ability_missing_key_is_error;
         "active complete ok" >:: test_active_ability_complete_no_error;
         "permanent no effect field"
         >:: test_permanent_ability_with_no_effect_field_is_error;
         "permanent with key" >:: test_permanent_ability_with_key_is_error;
         "timed missing activates_at"
         >:: test_timed_ability_missing_activates_at_is_error;
         "kill_unlocked missing kills"
         >:: test_kill_unlocked_missing_required_kills_is_error;
         "spread without directional"
         >:: test_spread_without_directional_shape_is_error;
       ]

let assign_tests =
  "Assign statement"
  >::: [
         "undefined target" >:: test_assign_to_undefined_target_is_error;
         "undefined ability" >:: test_assign_undefined_ability_is_error;
         "duplicate ability in call"
         >:: test_assign_duplicate_ability_in_same_call_is_error;
         "valid assign ok" >:: test_assign_valid_no_error;
       ]

let monster_tests =
  "Monster checks"
  >::: [
         "negative count" >:: test_monster_negative_count_is_error;
         "zero count ok" >:: test_monster_zero_count_no_error;
         "towards undefined player"
         >:: test_monster_towards_undefined_player_is_error;
         "towards defined player"
         >:: test_monster_towards_defined_player_no_error;
       ]

let spawn_tests =
  "Spawn statement"
  >::: [
         "undefined entity" >:: test_spawn_undefined_entity_is_error;
         "defined monster ok" >:: test_spawn_defined_monster_no_error;
         "undefined inside loop" >:: test_spawn_inside_loop_undefined_is_error;
       ]

let function_tests =
  "Function checks"
  >::: [
         "duplicate param" >:: test_fn_duplicate_param_is_error;
         "self recursion" >:: test_fn_self_recursion_is_error;
         "call undefined" >:: test_fn_call_undefined_is_error;
         "wrong arity" >:: test_fn_call_wrong_arity_is_error;
         "correct arity ok" >:: test_fn_call_correct_arity_no_error;
         "nested fn decl" >:: test_fn_nested_decl_is_error;
       ]

let entity_ref_tests =
  "Entity reference checks"
  >::: [
         "undefined entity field override"
         >:: test_field_override_on_undefined_entity_is_error;
         "defined monster field override"
         >:: test_field_override_on_defined_monster_no_error;
       ]

let () =
  run_test_tt_main
    ("GameDSL static analyzer"
    >::: [
           global_structure_tests;
           symbol_table_tests;
           key_conflict_tests;
           ability_type_tests;
           assign_tests;
           monster_tests;
           spawn_tests;
           function_tests;
           entity_ref_tests;
         ])
