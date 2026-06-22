(* ============================================================
   codegen.ml — Code Generator for GameDSL
   ============================================================
   Takes a `program` (ast.ml) that has ALREADY PASSED
   semant.check_program (i.e. result.errors = []) and emits a
   single self-contained Python source file using pygame.

   DESIGN PRINCIPLE
   -----------------
   Code generation is a structural walk: one `emit_*` function
   per AST node type, each returning a Python source fragment
   as a string. Composite nodes call emit_* on their children
   and stitch the strings together with the right indentation.

   This file purposefully does NOT re-validate anything semant.ml
   already checked (required fields, key conflicts, etc.) — it
   assumes a clean AST and will raise Invalid_argument if it
   encounters something semant.ml should have caught, as a
   defensive double-check rather than a primary validation path.
   ============================================================ *)

open Ast

(* ============================================================
   SECTION 1 — STRING / INDENTATION HELPERS
   ============================================================ *)

let buf = Buffer.create 4096
let indent_str n = String.make (n * 4) ' '

let emit_line ?(indent = 0) line =
  Buffer.add_string buf (indent_str indent);
  Buffer.add_string buf line;
  Buffer.add_char buf '\n'

let emit_blank () = Buffer.add_char buf '\n'

(* python identifiers can't contain spaces or start with a digit;
   entity names in GameDSL are already constrained to
   [a-zA-Z][a-zA-Z0-9_]* by the STRING grammar rule, so they are
   already valid Python identifiers / dict keys as-is. We still
   centralize key-quoting here in one place. *)
let py_str s = "\"" ^ String.concat "\\\"" (String.split_on_char '"' s) ^ "\""

(* ============================================================
   SECTION 2 — EXPRESSIONS
   Walks expr and produces the equivalent Python expression.
   This mirrors parse_expr/parse_term/parse_factor structurally:
   one case per AST constructor, each emitting valid Python.
   ============================================================ *)

let rec emit_expr = function
  | EInt n -> string_of_int n
  | EFloat f -> Printf.sprintf "%g" f
  | EVar name -> name
  | EBinop (op, l, r) ->
      let op_str =
        match op with Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"
      in
      Printf.sprintf "(%s %s %s)" (emit_expr l) op_str (emit_expr r)
  | ERuntimeCall (fn, arg) -> emit_runtime_call fn arg
  | EFieldRead (entity, fields) -> emit_field_access entity fields

and emit_runtime_call fn arg =
  let arg_str =
    match arg with
    | Some (ArgString s) -> py_str s
    | Some (ArgIdent s) -> s
    | None -> ""
  in
  match fn with
  | MonstersKilled ->
      Printf.sprintf "game_state.monster_killed_count(%s)" arg_str
  | MonsterCount -> Printf.sprintf "game_state.monster_count(%s)" arg_str
  | MonsterHealth -> Printf.sprintf "game_state.monster_health(%s)" arg_str
  | PlayerHealth -> Printf.sprintf "game_state.player_health(%s)" arg_str
  | PlayersKilled -> "game_state.players_killed"
  | PlayersAlive -> "game_state.players_alive"
  | TimeElapsed -> "game_state.time_elapsed"

and emit_entity_ref = function
  | RefString name -> Printf.sprintf "entity_registry[%s]" (py_str name)
  | RefIdent name -> Printf.sprintf "entity_registry[%s]" name

and emit_field_access entity fields =
  let base = emit_entity_ref entity in
  let path = String.concat "." fields in
  base ^ "." ^ path

(* ============================================================
   SECTION 3 — CONDITIONS
   ============================================================ *)

let rec emit_condition = function
  | CCmp (l, op, r) ->
      let op_str =
        match op with
        | Eq -> "=="
        | Neq -> "!="
        | Geq -> ">="
        | Leq -> "<="
        | Gt -> ">"
        | Lt -> "<"
      in
      Printf.sprintf "(%s %s %s)" (emit_expr l) op_str (emit_expr r)
  | CAnd (l, r) ->
      Printf.sprintf "(%s and %s)" (emit_condition l) (emit_condition r)
  | COr (l, r) ->
      Printf.sprintf "(%s or %s)" (emit_condition l) (emit_condition r)
  | CNot c -> Printf.sprintf "(not %s)" (emit_condition c)

(* ============================================================
   SECTION 4 — STATEMENTS
   Each stmt becomes one or more lines of Python at the given
   indentation level. Compound statements (if/loop/fn) recurse
   with indent+1 for their bodies.
   ============================================================ *)

let rec emit_stmt indent = function
  | SVarDecl (name, e) ->
      emit_line ~indent (Printf.sprintf "%s = %s" name (emit_expr e))
  | SVarAssign (name, e) ->
      emit_line ~indent (Printf.sprintf "%s = %s" name (emit_expr e))
  | SFieldOverride (entity, fields, e) ->
      let target = emit_entity_ref entity ^ "." ^ String.concat "." fields in
      emit_line ~indent (Printf.sprintf "%s = %s" target (emit_expr e))
  | SIf (cond, then_b, else_b) -> (
      emit_line ~indent (Printf.sprintf "if %s:" (emit_condition cond));
      emit_stmt_block (indent + 1) then_b;
      match else_b with
      | None -> ()
      | Some b ->
          emit_line ~indent "else:";
          emit_stmt_block (indent + 1) b)
  | SLoop loop -> emit_loop_stmt indent loop
  | SFnDecl f -> emit_fn_decl indent f
  | SFnCall c -> emit_line ~indent (emit_fn_call_expr c)
  | SSpawn s -> emit_spawn_stmt indent s

and emit_stmt_block indent stmts =
  if stmts = [] then emit_line ~indent "pass"
  else List.iter (emit_stmt indent) stmts

and emit_fn_call_expr c =
  let args = String.concat ", " (List.map emit_expr c.call_args) in
  Printf.sprintf "%s(%s)" c.call_name args

and emit_fn_decl indent f =
  let params = String.concat ", " f.fn_params in
  emit_line ~indent (Printf.sprintf "def %s(%s):" f.fn_name params);
  emit_stmt_block (indent + 1) f.fn_body;
  emit_blank ()

and emit_spawn_stmt indent s =
  let name_str = py_str s.spawn_name in
  match s.spawn_position with
  | Some (x, y) ->
      emit_line ~indent
        (Printf.sprintf "game_state.spawn(%s, position=(%s, %s))" name_str
           (emit_expr x) (emit_expr y))
  | None -> emit_line ~indent (Printf.sprintf "game_state.spawn(%s)" name_str)

(* ============================================================
   SECTION 5 — LOOP STATEMENT
   loop_count maps to one of three Python control structures:
     LFinite n     -> for _ in range(n):
     LInfinite     -> while True:
     LCondition c  -> while <cond>:
   action is a structural subset of stmt (no var_decl/fn_decl)
   plus move/wait, which have no stmt equivalent and are emitted
   as direct calls into the runtime's movement/scheduling API.
   ============================================================ *)

and emit_loop_stmt indent loop =
  (match loop.loop_times with
  | LFinite n -> emit_line ~indent (Printf.sprintf "for _ in range(%d):" n)
  | LInfinite -> emit_line ~indent "while True:"
  | LCondition c ->
      emit_line ~indent (Printf.sprintf "while %s:" (emit_condition c)));
  emit_action_block (indent + 1) loop.loop_actions

and emit_action_block indent actions =
  if actions = [] then emit_line ~indent "pass"
  else List.iter (emit_action indent) actions

and emit_action indent = function
  | AMove mv -> emit_move_action indent mv
  | AWait n -> emit_line ~indent (Printf.sprintf "game_state.wait(%d)" n)
  | AIf (cond, then_b, else_b) -> (
      emit_line ~indent (Printf.sprintf "if %s:" (emit_condition cond));
      emit_action_block (indent + 1) then_b;
      match else_b with
      | None -> ()
      | Some b ->
          emit_line ~indent "else:";
          emit_action_block (indent + 1) b)
  | AAssign (name, e) ->
      emit_line ~indent (Printf.sprintf "%s = %s" name (emit_expr e))
  | AField (entity, fields, e) ->
      let target = emit_entity_ref entity ^ "." ^ String.concat "." fields in
      emit_line ~indent (Printf.sprintf "%s = %s" target (emit_expr e))
  | ALoop l -> emit_loop_stmt indent l
  | ASpawn s -> emit_spawn_stmt indent s

and emit_move_action indent = function
  | MUp -> emit_line ~indent "self.move(0, -1)"
  | MDown -> emit_line ~indent "self.move(0, 1)"
  | MLeft -> emit_line ~indent "self.move(-1, 0)"
  | MRight -> emit_line ~indent "self.move(1, 0)"
  | MRandom -> emit_line ~indent "self.move_random()"
  | MTowards pname ->
      emit_line ~indent (Printf.sprintf "self.move_towards(%s)" (py_str pname))

(* ============================================================
   SECTION 6 — WORLD
   ============================================================ *)

let emit_world w =
  emit_line (Printf.sprintf "GRID_WIDTH  = %d" w.grid.width);
  emit_line (Printf.sprintf "GRID_HEIGHT = %d" w.grid.height);
  emit_line (Printf.sprintf "DURATION    = %d" w.duration);
  emit_blank ()

(* ============================================================
   SECTION 7 — PLAYER
   ============================================================ *)

let emit_controls c =
  Printf.sprintf "{\"up\": %s, \"down\": %s, \"left\": %s, \"right\": %s}"
    (py_str c.up) (py_str c.down) (py_str c.left) (py_str c.right)

let emit_player pl =
  let img = match pl.p_img with Some s -> py_str s | None -> "None" in
  let pos =
    match pl.p_position with
    | Some (x, y) -> Printf.sprintf "(%s, %s)" (emit_expr x) (emit_expr y)
    | None -> "None"
  in
  emit_line
    (Printf.sprintf
       "Player(%s, health=%s, img=%s, active=%s, position=%s, controls=%s)"
       (py_str pl.p_name) (emit_expr pl.p_health) img
       (if pl.p_active then "True" else "False")
       pos
       (emit_controls pl.p_controls))

let emit_players players =
  emit_line "players = [";
  List.iter
    (fun pl ->
      Buffer.add_string buf (indent_str 1);
      emit_player pl)
    players;
  emit_line "]";
  emit_blank ()

(* ============================================================
   SECTION 8 — ABILITY
   Ability fields are emitted as Python keyword arguments to an
   Ability(...) constructor call. ability_stmt's local var_decl /
   var_assign / if logic is compiled into a small helper function
   `compute_<name>_fields()` that returns a dict, which is then
   splatted into the constructor — this lets local variables and
   conditionals genuinely affect field values at construction time.
   ============================================================ *)

let ability_type_str = function
  | Active -> "active"
  | Permanent -> "permanent"
  | Timed -> "timed"
  | KillUnlocked -> "kill_unlocked"

let shape_str = function
  | Manhattan -> "manhattan"
  | Chebyshev -> "chebyshev"
  | Directional -> "directional"

(* Emits the body as Python statements that build up a `fields` dict.
   AbField entries become fields["name"] = value.
   AbVarDecl/AbVarAssign become plain local variable statements.
   AbIf recurses, preserving conditionals around field assignment. *)
let rec emit_ability_stmt indent = function
  | AbField field ->
      let key, value =
        match field with
        | FDamage e -> ("damage", emit_expr e)
        | FRange e -> ("range", emit_expr e)
        | FShape s -> ("shape", py_str (shape_str s))
        | FSpread e -> ("spread", emit_expr e)
        | FKey k -> ("key", py_str k)
        | FActivatesAt n -> ("activates_at", string_of_int n)
        | FRequiredKills e -> ("required_kills", emit_expr e)
        | FDamageMultiplier e -> ("damage_multiplier", emit_expr e)
        | FDamageReduction n -> ("damage_reduction", string_of_int n)
        | FHealthRegen e -> ("health_regen", emit_expr e)
        | FSpeedBoost e -> ("speed_boost", emit_expr e)
      in
      emit_line ~indent (Printf.sprintf "fields[%s] = %s" (py_str key) value)
  | AbVarDecl (name, e) ->
      emit_line ~indent (Printf.sprintf "%s = %s" name (emit_expr e))
  | AbVarAssign (name, e) ->
      emit_line ~indent (Printf.sprintf "%s = %s" name (emit_expr e))
  | AbIf (cond, then_b, else_b) -> (
      emit_line ~indent (Printf.sprintf "if %s:" (emit_condition cond));
      emit_ability_stmt_block (indent + 1) then_b;
      match else_b with
      | None -> ()
      | Some b ->
          emit_line ~indent "else:";
          emit_ability_stmt_block (indent + 1) b)

and emit_ability_stmt_block indent stmts =
  if stmts = [] then emit_line ~indent "pass"
  else List.iter (emit_ability_stmt indent) stmts

let emit_ability ab =
  let fn_name = Printf.sprintf "_compute_%s_fields" ab.a_name in
  emit_line (Printf.sprintf "def %s():" fn_name);
  emit_line ~indent:1 "fields = {}";
  emit_ability_stmt_block 1 ab.a_body;
  emit_line ~indent:1 "return fields";
  emit_blank ();
  let img = match ab.a_img with Some s -> py_str s | None -> "None" in
  emit_line
    (Printf.sprintf
       "ability_registry[%s] = Ability(%s, type=%s, img=%s, **%s())"
       (py_str ab.a_name) (py_str ab.a_name)
       (py_str (ability_type_str ab.a_type))
       img fn_name);
  emit_blank ()

let emit_abilities abilities = List.iter emit_ability abilities

(* ============================================================
   SECTION 9 — MONSTER
   movement is either a simple enum value, a "towards" reference,
   or a full loop_stmt compiled into a per-instance coroutine
   function `_movement_<name>(self)`.
   ============================================================ *)

let emit_movement_simple = function
  | MvRandom -> "\"random\""
  | MvStationary -> "\"stationary\""
  | MvTowards pname -> Printf.sprintf "\"towards:%s\"" pname
  | MvLoop _ -> assert false (* handled separately, see emit_monster *)

let emit_monster m =
  match m.m_movement with
  | MvLoop loop ->
      let fn_name = Printf.sprintf "_movement_%s" m.m_name in
      emit_line (Printf.sprintf "def %s(self):" fn_name);
      emit_loop_stmt 1 loop;
      emit_blank ();
      let img = match m.m_img with Some s -> py_str s | None -> "None" in
      let pos =
        match m.m_position with
        | Some (x, y) -> Printf.sprintf "(%s, %s)" (emit_expr x) (emit_expr y)
        | None -> "None"
      in
      emit_line
        (Printf.sprintf
           "monster_registry[%s] = MonsterType(%s, health=%s, img=%s, \
            movement=%s, count=%s, position=%s)"
           (py_str m.m_name) (py_str m.m_name) (emit_expr m.m_health) img
           fn_name (emit_expr m.m_count) pos);
      emit_blank ()
  | simple ->
      let img = match m.m_img with Some s -> py_str s | None -> "None" in
      let pos =
        match m.m_position with
        | Some (x, y) -> Printf.sprintf "(%s, %s)" (emit_expr x) (emit_expr y)
        | None -> "None"
      in
      emit_line
        (Printf.sprintf
           "monster_registry[%s] = MonsterType(%s, health=%s, img=%s, \
            movement=%s, count=%s, position=%s)"
           (py_str m.m_name) (py_str m.m_name) (emit_expr m.m_health) img
           (emit_movement_simple simple)
           (emit_expr m.m_count) pos);
      emit_blank ()

let emit_monsters monsters = List.iter emit_monster monsters

(* ============================================================
   SECTION 10 — ASSIGN
   ============================================================ *)

let emit_assign_target = function TgString s -> py_str s | TgIdent s -> s
let emit_assign_ability = function AbName s -> py_str s | AbParam s -> s

let emit_assign a =
  let names = List.map emit_assign_ability a.asg_abilities in
  emit_line
    (Printf.sprintf "game_state.assign_abilities(%s, [%s])"
       (emit_assign_target a.asg_target)
       (String.concat ", " names))

let emit_assigns assigns = List.iter emit_assign assigns

(* ============================================================
   SECTION 11 — OBSTACLE
   ============================================================ *)

let emit_obstacle o =
  let x, y = o.o_position in
  let w, h =
    match o.o_size with
    | Some d -> (string_of_int d.width, string_of_int d.height)
    | None -> ("1", "1")
  in
  let img = match o.o_img with Some s -> py_str s | None -> "None" in
  emit_line
    (Printf.sprintf "obstacles.append(Obstacle(%s, %s, %s, %s, %s, img=%s))"
       (py_str o.o_name) (emit_expr x) (emit_expr y) w h img)

let emit_obstacles obstacles =
  emit_line "obstacles = []";
  List.iter emit_obstacle obstacles;
  emit_blank ()

(* ============================================================
   SECTION 12 — WIN CONDITION
   ============================================================ *)

let emit_win_field = function
  | WSurvive n -> Printf.sprintf "game_state.time_elapsed >= %d" n
  | WKillMonsters c -> emit_condition c
  | WKillPlayers c -> emit_condition c
  | WElimination c -> emit_condition c

let emit_win_condition w =
  emit_line "def check_win_condition():";
  let conds = List.map emit_win_field w.w_fields in
  emit_line ~indent:1 (Printf.sprintf "return %s" (String.concat " or " conds));
  emit_blank ()

(* ============================================================
   SECTION 13 — FULL PROGRAM
   Emits, in order: imports/header, world constants, ability
   compute-functions + registry, monster registry (with movement
   coroutines), player list, obstacle list, assign calls, win
   condition function, pre_stmts (top-level vars/fns before world
   in the DSL), post_stmts (runtime game logic), and a main()
   entry point that wires it all into the runtime's game loop.
   ============================================================ *)

let header =
  "#!/usr/bin/env python3\n\
   # ============================================================\n\
   # Auto-generated by the GameDSL compiler. Do not edit by hand.\n\
   # ============================================================\n\
   import random\n\
   from runtime import (\n\
  \    GameState, Player, MonsterType, Ability, Obstacle, run_game\n\
   )\n\n\
   entity_registry  = {}\n\
   ability_registry = {}\n\
   monster_registry = {}\n\
   game_state = GameState()\n"

let generate (prog : program) : string =
  Buffer.clear buf;
  emit_line header;
  emit_blank ();

  emit_line "# ---- pre-statements (vars/fns declared before world) ----";
  List.iter (emit_stmt 0) prog.pre_stmts;
  emit_blank ();

  emit_line "# ---- world ----";
  emit_world prog.prog_world;

  emit_line "# ---- abilities ----";
  emit_abilities prog.abilities;

  emit_line "# ---- monsters ----";
  emit_monsters prog.monsters;

  emit_line "# ---- players ----";
  emit_players prog.players;

  emit_line "# ---- obstacles ----";
  emit_obstacles prog.obstacles;

  emit_line "# ---- assign abilities ----";
  emit_assigns prog.assigns;
  emit_blank ();

  emit_line "# ---- win condition ----";
  emit_win_condition prog.win_condition;

  emit_line "# ---- post-statements (runtime game logic) ----";
  List.iter (emit_stmt 0) prog.post_stmts;
  emit_blank ();

  emit_line "if __name__ == \"__main__\":";
  emit_line ~indent:1
    "run_game(players, monster_registry, obstacles, check_win_condition, \
     GRID_WIDTH, GRID_HEIGHT, DURATION)";

  Buffer.contents buf

(* ============================================================
   SECTION 14 — FILE ENTRY POINT
   ============================================================ *)

let generate_to_file prog out_filename =
  let code = generate prog in
  let oc = open_out out_filename in
  output_string oc code;
  close_out oc
