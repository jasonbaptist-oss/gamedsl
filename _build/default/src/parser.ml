(* ============================================================
   parser.ml — Recursive Descent Parser for GameDSL
   ============================================================
   Consumes the token list produced by lexer.ml (tokenize)
   and produces a program (ast.ml) value.
   ============================================================ *)

open Lexer
open Ast

(* ============================================================
   SECTION 1 — PARSER STATE AND ERRORS
   ============================================================ *)

exception ParseError of string * position

let parse_error msg pos = raise (ParseError (msg, pos))

type parser_state = { toks : located_token array; mutable idx : int }

let make_parser tokens = { toks = Array.of_list tokens; idx = 0 }

let cur p = p.toks.(p.idx).token
let cur_pos p = p.toks.(p.idx).pos

let peek_next p =
  if p.idx + 1 < Array.length p.toks then p.toks.(p.idx + 1).token else EOF

let advance p = if p.idx < Array.length p.toks - 1 then p.idx <- p.idx + 1

let expect p expected =
  let got = cur p in
  if got = expected then advance p
  else
    parse_error
      (Printf.sprintf "Expected %s but found %s" (token_to_string expected)
         (token_to_string got))
      (cur_pos p)

let expect_ident p =
  match cur p with
  | IDENT s ->
      advance p;
      s
  | t ->
      parse_error
        (Printf.sprintf "Expected identifier but found %s" (token_to_string t))
        (cur_pos p)

let expect_string p =
  match cur p with
  | STRING_LIT s ->
      advance p;
      s
  | t ->
      parse_error
        (Printf.sprintf "Expected string literal but found %s"
           (token_to_string t))
        (cur_pos p)

let expect_field p =
  match cur p with
  | IDENT s -> advance p; s
  | HEALTH -> advance p; "health"
  | DAMAGE -> advance p; "damage"
  | RANGE -> advance p; "range"
  | SHAPE -> advance p; "shape"
  | SPREAD -> advance p; "spread"
  | MOVEMENT -> advance p; "movement"
  | COUNT -> advance p; "count"
  | ACTIVE -> advance p; "active"
  | POSITION -> advance p; "position"
  | IMG -> advance p; "img"
  | SIZE -> advance p; "size"
  | KEY_FIELD -> advance p; "key"
  | ACTIVATES_AT -> advance p; "activates_at"
  | REQUIRED_KILLS -> advance p; "required_kills"
  | DAMAGE_MULTIPLIER -> advance p; "damage_multiplier"
  | DAMAGE_REDUCTION -> advance p; "damage_reduction"
  | HEALTH_REGEN -> advance p; "health_regen"
  | SPEED_BOOST -> advance p; "speed_boost"
  | t ->
      parse_error
        (Printf.sprintf "Expected a field name but found %s" (token_to_string t))
        (cur_pos p)

let expect_filepath p =
  match cur p with
  | FILEPATH_LIT s ->
      advance p;
      s
  | STRING_LIT s ->
      advance p;
      s
  | t ->
      parse_error
        (Printf.sprintf "Expected filepath but found %s" (token_to_string t))
        (cur_pos p)

let expect_key p =
  match cur p with
  | KEY_LIT k ->
      advance p;
      k
  | STRING_LIT k ->
      advance p;
      k
  | t ->
      parse_error
        (Printf.sprintf "Expected a KEY value (like \"w\") but found %s"
           (token_to_string t))
        (cur_pos p)

(* ============================================================
   SECTION 2 — EXPRESSIONS
   ============================================================ *)

let rec parse_expr p =
  let left = ref (parse_term p) in
  let continue_loop = ref true in
  while !continue_loop do
    match cur p with
    | PLUS ->
        advance p;
        let right = parse_term p in
        left := EBinop (Add, !left, right)
    | MINUS ->
        advance p;
        let right = parse_term p in
        left := EBinop (Sub, !left, right)
    | _ -> continue_loop := false
  done;
  !left

and parse_term p =
  let left = ref (parse_factor p) in
  let continue_loop = ref true in
  while !continue_loop do
    match cur p with
    | STAR ->
        advance p;
        let right = parse_factor p in
        left := EBinop (Mul, !left, right)
    | SLASH ->
        advance p;
        let right = parse_factor p in
        left := EBinop (Div, !left, right)
    | _ -> continue_loop := false
  done;
  !left

and parse_factor p =
  match cur p with
  | INT_LIT n ->
      advance p;
      EInt n
  | FLOAT_LIT f ->
      advance p;
      EFloat f
  | LPAREN ->
      advance p;
      let e = parse_expr p in
      expect p RPAREN;
      e
  | MONSTERS_KILLED -> parse_runtime_call p MonstersKilled
  | MONSTER_COUNT -> parse_runtime_call p MonsterCount
  | MONSTER_HEALTH -> parse_runtime_call p MonsterHealth
  | PLAYER_HEALTH -> parse_runtime_call p PlayerHealth
  | PLAYERS_KILLED ->
      advance p;
      ERuntimeCall (PlayersKilled, None)
  | PLAYERS_ALIVE ->
      advance p;
      ERuntimeCall (PlayersAlive, None)
  | TIME_ELAPSED ->
      advance p;
      ERuntimeCall (TimeElapsed, None)
  | IDENT name ->
      advance p;
      if cur p = DOT then parse_field_read_tail p (RefIdent name)
      else if cur p = LPAREN then EVar name
      else EVar name
  | STRING_LIT s ->
      advance p;
      if cur p = DOT then parse_field_read_tail p (RefString s)
      else EString s    (* <-- Return it as an expression instead of erroring! *)
  | t ->
      parse_error
        (Printf.sprintf "Unexpected token in expression: %s" (token_to_string t))
        (cur_pos p)

and parse_runtime_call p fn =
  advance p;
  expect p LPAREN;
  let arg =
    match cur p with
    | STRING_LIT s ->
        advance p;
        ArgString s
    | IDENT s ->
        advance p;
        ArgIdent s
    | t ->
        parse_error
          (Printf.sprintf
             "Expected STRING or IDENT as runtime call argument, found %s"
             (token_to_string t))
          (cur_pos p)
  in
  expect p RPAREN;
  ERuntimeCall (fn, Some arg)

and parse_field_read_tail p entity =
  let fields = ref [] in
  while cur p = DOT do
    advance p;
    fields := expect_field p :: !fields
  done;
  EFieldRead (entity, List.rev !fields)

(* ============================================================
   SECTION 3 — CONDITIONS
   ============================================================ *)

let rec parse_condition p = parse_or_cond p

and parse_or_cond p =
  let left = ref (parse_and_cond p) in
  while cur p = OR do
    advance p;
    let right = parse_and_cond p in
    left := COr (!left, right)
  done;
  !left

and parse_and_cond p =
  let left = ref (parse_not_cond p) in
  while cur p = AND do
    advance p;
    let right = parse_not_cond p in
    left := CAnd (!left, right)
  done;
  !left

and parse_not_cond p =
  match cur p with
  | NOT ->
      advance p;
      let inner = parse_not_cond p in
      CNot inner
  | LPAREN ->
      advance p;
      let inner = parse_condition p in
      expect p RPAREN;
      inner
  | _ -> parse_comparison p

and parse_comparison p =
  let left = parse_expr p in
  let op =
    match cur p with
    | EQ -> advance p; Eq
    | NEQ -> advance p; Neq
    | GEQ -> advance p; Geq
    | LEQ -> advance p; Leq
    | GT -> advance p; Gt
    | LT -> advance p; Lt
    | t ->
        parse_error
          (Printf.sprintf "Expected a comparison operator but found %s"
             (token_to_string t))
          (cur_pos p)
  in
  let right = parse_expr p in
  CCmp (left, op, right)

(* ============================================================
   SECTION 4 — ENTITY REFERENCES
   ============================================================ *)

let parse_entity_ref p =
  match cur p with
  | STRING_LIT s -> advance p; RefString s
  | IDENT s -> advance p; RefIdent s
  | t ->
      parse_error
        (Printf.sprintf
           "Expected entity reference (STRING or IDENT) but found %s"
           (token_to_string t))
        (cur_pos p)

let parse_field_chain p =
  let fields = ref [] in
  expect p DOT;
  fields := expect_field p :: !fields;
  while cur p = DOT do
    advance p;
    fields := expect_field p :: !fields
  done;
  List.rev !fields

(* ============================================================
   SECTION 5 — STATEMENTS
   ============================================================ *)

let rec parse_stmt p =
  match cur p with
  | VAR -> parse_var_decl p
  | IF -> parse_if_stmt p
  | LOOP -> SLoop (parse_loop_stmt p)
  | FN -> parse_fn_decl p
  | SPAWN -> SSpawn (parse_spawn_stmt p)
  | IDENT _ | STRING_LIT _ -> (
      let saved_idx = p.idx in
      let entity = parse_entity_ref p in
      match cur p with
      | DOT -> (
          let fields = parse_field_chain p in
          expect p ASSIGN_OP;
          let value = parse_expr p in
          match entity with
          | RefString _ | RefIdent _ -> SFieldOverride (entity, fields, value))
      | ASSIGN_OP -> (
          match entity with
          | RefIdent name ->
              advance p;
              let value = parse_expr p in
              SVarAssign (name, value)
          | RefString _ ->
              parse_error "Cannot assign directly to a string literal"
                (cur_pos p))
      | LPAREN -> (
          match entity with
          | RefIdent name ->
              let args = parse_call_args p in
              SFnCall { call_name = name; call_args = args }
          | RefString _ ->
              p.idx <- saved_idx;
              parse_error "Unexpected '(' after string literal" (cur_pos p))
      | t ->
          p.idx <- saved_idx;
          parse_error
            (Printf.sprintf
               "Expected '.', '=', or '(' after identifier, found %s"
               (token_to_string t))
            (cur_pos p))
  | t ->
      parse_error
        (Printf.sprintf "Unexpected token at start of statement: %s"
           (token_to_string t))
        (cur_pos p)

and parse_var_decl p =
  expect p VAR;
  let name = expect_ident p in
  expect p ASSIGN_OP;
  let value = parse_expr p in
  SVarDecl (name, value)

and parse_if_stmt p =
  expect p IF;
  expect p LPAREN;
  let cond = parse_condition p in
  expect p RPAREN;
  expect p LBRACE;
  let then_body = parse_stmt_list_until p RBRACE in
  expect p RBRACE;
  let else_body =
    if cur p = ELSE then begin
      advance p;
      expect p LBRACE;
      let body = parse_stmt_list_until p RBRACE in
      expect p RBRACE;
      Some body
    end
    else None
  in
  SIf (cond, then_body, else_body)

and parse_stmt_list_until p terminator =
  let stmts = ref [] in
  while cur p <> terminator do
    stmts := parse_stmt p :: !stmts
  done;
  List.rev !stmts

and parse_fn_decl p =
  expect p FN;
  let name = expect_ident p in
  expect p LPAREN;
  let params =
    if cur p = RPAREN then []
    else begin
      let ps = ref [ expect_ident p ] in
      while cur p = COMMA do
        advance p;
        ps := expect_ident p :: !ps
      done;
      List.rev !ps
    end
  in
  expect p RPAREN;
  expect p LBRACE;
  let body = parse_stmt_list_until p RBRACE in
  expect p RBRACE;
  SFnDecl { fn_name = name; fn_params = params; fn_body = body }

and parse_call_args p =
  expect p LPAREN;
  let args =
    if cur p = RPAREN then []
    else begin
      let es = ref [ parse_expr p ] in
      while cur p = COMMA do
        advance p;
        es := parse_expr p :: !es
      done;
      List.rev !es
    end
  in
  expect p RPAREN;
  args

and parse_spawn_stmt p =
  expect p SPAWN;
  let name = expect_string p in
  let position =
    if cur p = POSITION then begin
      advance p;
      expect p COLON;
      let x = parse_expr p in
      expect p COMMA;
      let y = parse_expr p in
      Some (x, y)
    end
    else None
  in
  { spawn_name = name; spawn_position = position }

(* ============================================================
   SECTION 6 — LOOP STATEMENT
   ============================================================ *)

and parse_action_list_until p terminator =
  let acts = ref [] in
  while cur p <> terminator do
    acts := parse_action p :: !acts
  done;
  List.rev !acts

and parse_loop_stmt p =
  expect p LOOP;
  expect p LBRACE;
  expect p TIMES;
  expect p COLON;
  let times =
    match cur p with
    | INT_LIT n ->
        advance p;
        LFinite n
    | INFINITE ->
        advance p;
        LInfinite
    | _ -> LCondition (parse_condition p)
  in
  expect p ACTIONS;
  expect p LBRACE;
  let actions = parse_action_list_until p RBRACE in
  expect p RBRACE;
  expect p RBRACE;
  { loop_times = times; loop_actions = actions }

and parse_action p =
  match cur p with
  | MOVE -> parse_move_action p
  | WAIT -> parse_wait_action p
  | IF -> parse_if_action p
  | LOOP -> ALoop (parse_loop_stmt p)
  | SPAWN -> ASpawn (parse_spawn_stmt p)
  | IDENT _ | STRING_LIT _ -> (
      let saved_idx = p.idx in
      let entity = parse_entity_ref p in
      match cur p with
      | DOT ->
          let fields = parse_field_chain p in
          expect p ASSIGN_OP;
          let value = parse_expr p in
          AField (entity, fields, value)
      | ASSIGN_OP -> (
          match entity with
          | RefIdent name ->
              advance p;
              let value = parse_expr p in
              AAssign (name, value)
          | RefString _ ->
              p.idx <- saved_idx;
              parse_error
                "Cannot assign directly to a string literal in an action"
                (cur_pos p))
      | LPAREN -> (
          match entity with
          | RefIdent name ->
              let args = parse_call_args p in
              AFnCall { call_name = name; call_args = args }
          | RefString _ ->
              p.idx <- saved_idx;
              parse_error "Unexpected '(' after string literal" (cur_pos p))
      | t ->
          p.idx <- saved_idx;
          parse_error
            (Printf.sprintf
               "Expected '.', '=', or '(' after identifier in action, found %s"
               (token_to_string t))
            (cur_pos p))
  | t ->
      parse_error
        (Printf.sprintf "Unexpected token at start of action: %s"
           (token_to_string t))
        (cur_pos p)

and parse_move_action p =
  expect p MOVE;
  expect p COLON;
  let mv =
    match cur p with
    | UP -> advance p; MUp
    | DOWN -> advance p; MDown
    | LEFT -> advance p; MLeft
    | RIGHT -> advance p; MRight
    | RANDOM -> advance p; MRandom
    | TOWARDS ->
        advance p;
        let name = expect_string p in
        MTowards name
    | t ->
        parse_error
          (Printf.sprintf "Expected a move value, found %s" (token_to_string t))
          (cur_pos p)
  in
  AMove mv

and parse_wait_action p =
  expect p WAIT;
  expect p COLON;
  match cur p with
  | DURATION_LIT n ->
      advance p;
      AWait n
  | t ->
      parse_error
        (Printf.sprintf "Expected a duration like 5s, found %s"
           (token_to_string t))
        (cur_pos p)

and parse_if_action p =
  expect p IF;
  expect p LPAREN;
  let cond = parse_condition p in
  expect p RPAREN;
  expect p LBRACE;
  let then_body = parse_action_list_until p RBRACE in
  expect p RBRACE;
  let else_body =
    if cur p = ELSE then begin
      advance p;
      expect p LBRACE;
      let body = parse_action_list_until p RBRACE in
      expect p RBRACE;
      Some body
    end
    else None
  in
  AIf (cond, then_body, else_body)

(* ============================================================
   SECTION 7 — WORLD BLOCK
   ============================================================ *)

let parse_world p =
  expect p WORLD;
  expect p LBRACE;
  expect p GRID_SIZE;
  expect p COLON;
  let grid =
    match cur p with
    | GRID_DIM_LIT (w, h) ->
        advance p;
        { width = w; height = h }
    | t ->
        parse_error
          (Printf.sprintf "Expected grid dimension like 20x15, found %s"
             (token_to_string t))
          (cur_pos p)
  in
  expect p DURATION_KW;
  expect p COLON;
  let duration =
    match cur p with
    | DURATION_LIT n -> advance p; n
    | t ->
        parse_error
          (Printf.sprintf "Expected a duration like 120s, found %s"
             (token_to_string t))
          (cur_pos p)
  in
  expect p RBRACE;
  { grid; duration }

(* ============================================================
   SECTION 8 — PLAYER BLOCK
   ============================================================ *)

let parse_position_pair p =
  expect p COLON;
  let x = parse_expr p in
  expect p COMMA;
  let y = parse_expr p in
  (x, y)

let parse_player p =
  expect p PLAYER;
  let name = expect_string p in
  expect p LBRACE;

  expect p HEALTH;
  expect p COLON;
  let health = parse_expr p in

  let img =
    if cur p = IMG then begin
      advance p;
      expect p COLON;
      Some (expect_filepath p)
    end
    else None
  in

  let active =
    if cur p = ACTIVE then begin
      advance p;
      expect p COLON;
      match cur p with
      | TRUE -> advance p; true
      | FALSE -> advance p; false
      | t ->
          parse_error
            (Printf.sprintf "Expected true or false, found %s"
               (token_to_string t))
            (cur_pos p)
    end
    else true
  in

  let position =
    if cur p = POSITION then begin
      advance p;
      Some (parse_position_pair p)
    end
    else None
  in

  expect p CONTROLS;
  expect p LBRACE;
  let up = ref "" and down = ref "" and left = ref "" and right = ref "" in
  let seen = ref 0 in
  while cur p <> RBRACE do
    (match cur p with
    | UP ->
        advance p;
        expect p COLON;
        up := expect_key p
    | DOWN ->
        advance p;
        expect p COLON;
        down := expect_key p
    | LEFT ->
        advance p;
        expect p COLON;
        left := expect_key p
    | RIGHT ->
        advance p;
        expect p COLON;
        right := expect_key p
    | t ->
        parse_error
          (Printf.sprintf "Expected up/down/left/right, found %s"
             (token_to_string t))
          (cur_pos p));
    incr seen
  done;
  expect p RBRACE;
  expect p RBRACE;

  {
    p_name = name;
    p_health = health;
    p_img = img;
    p_active = active;
    p_position = position;
    p_controls = { up = !up; down = !down; left = !left; right = !right };
  }

(* ============================================================
   SECTION 9 — ABILITY BLOCK
   ============================================================ *)

let parse_shape_val p =
  match cur p with
  | MANHATTAN -> advance p; Manhattan
  | CHEBYSHEV -> advance p; Chebyshev
  | DIRECTIONAL -> advance p; Directional
  | t ->
      parse_error
        (Printf.sprintf "Expected a shape value, found %s" (token_to_string t))
        (cur_pos p)

let rec parse_ability_stmt p =
  match cur p with
  | DAMAGE ->
      advance p;
      expect p COLON;
      AbField (FDamage (parse_expr p))
  | RANGE ->
      advance p;
      expect p COLON;
      AbField (FRange (parse_expr p))
  | SHAPE ->
      advance p;
      expect p COLON;
      AbField (FShape (parse_shape_val p))
  | SPREAD ->
      advance p;
      expect p COLON;
      AbField (FSpread (parse_expr p))
  | KEY_FIELD ->
      advance p;
      expect p COLON;
      AbField (FKey (expect_key p))
  | ACTIVATES_AT -> (
      advance p;
      expect p COLON;
      match cur p with
      | DURATION_LIT n -> advance p; AbField (FActivatesAt n)
      | t ->
          parse_error
            (Printf.sprintf "Expected a duration, found %s" (token_to_string t))
            (cur_pos p))
  | REQUIRED_KILLS ->
      advance p;
      expect p COLON;
      AbField (FRequiredKills (parse_expr p))
  | DAMAGE_MULTIPLIER ->
      advance p;
      expect p COLON;
      AbField (FDamageMultiplier (parse_expr p))
  | DAMAGE_REDUCTION -> (
      advance p;
      expect p COLON;
      match cur p with
      | PERCENT_LIT n -> advance p; AbField (FDamageReduction n)
      | t ->
          parse_error
            (Printf.sprintf "Expected a percent like 30%%, found %s"
               (token_to_string t))
            (cur_pos p))
  | HEALTH_REGEN ->
      advance p;
      expect p COLON;
      AbField (FHealthRegen (parse_expr p))
  | SPEED_BOOST ->
      advance p;
      expect p COLON;
      AbField (FSpeedBoost (parse_expr p))
  | VAR -> (
      match parse_var_decl p with
      | SVarDecl (n, e) -> AbVarDecl (n, e)
      | _ -> assert false)
  | IF ->
      expect p IF;
      expect p LPAREN;
      let cond = parse_condition p in
      expect p RPAREN;
      expect p LBRACE;
      let then_body = parse_ability_stmt_list_until p RBRACE in
      expect p RBRACE;
      let else_body =
        if cur p = ELSE then begin
          advance p;
          expect p LBRACE;
          let b = parse_ability_stmt_list_until p RBRACE in
          expect p RBRACE;
          Some b
        end
        else None
      in
      AbIf (cond, then_body, else_body)
  | IDENT name ->
      advance p;
      expect p ASSIGN_OP;
      let value = parse_expr p in
      AbVarAssign (name, value)
  | t ->
      parse_error
        (Printf.sprintf "Unexpected token in ability body: %s"
           (token_to_string t))
        (cur_pos p)

and parse_ability_stmt_list_until p terminator =
  let stmts = ref [] in
  while cur p <> terminator do
    stmts := parse_ability_stmt p :: !stmts
  done;
  List.rev !stmts

let parse_ability_type p =
  match cur p with
  | ACTIVE -> advance p; Active
  | PERMANENT -> advance p; Permanent
  | TIMED -> advance p; Timed
  | KILL_UNLOCKED -> advance p; KillUnlocked
  | t ->
      parse_error
        (Printf.sprintf "Expected an ability type, found %s" (token_to_string t))
        (cur_pos p)

let parse_ability p =
  expect p ABILITY;
  let name = expect_string p in
  expect p LBRACE;
  expect p TYPE_KW;
  expect p COLON;
  let atype = parse_ability_type p in
  let img =
    if cur p = IMG then begin
      advance p;
      expect p COLON;
      Some (expect_filepath p)
    end
    else None
  in
  let body = parse_ability_stmt_list_until p RBRACE in
  expect p RBRACE;
  { a_name = name; a_type = atype; a_img = img; a_body = body }

(* ============================================================
   SECTION 10 — MONSTER BLOCK
   ============================================================ *)

let parse_movement p =
  expect p MOVEMENT;
  expect p COLON;
  match cur p with
  | TOWARDS ->
      advance p;
      let name = expect_string p in
      MvTowards name
  | RANDOM -> advance p; MvRandom
  | STATIONARY -> advance p; MvStationary
  | LOOP -> MvLoop (parse_loop_stmt p)
  | t ->
      parse_error
        (Printf.sprintf "Expected a movement value, found %s"
           (token_to_string t))
        (cur_pos p)

let parse_monster p =
  expect p MONSTER;
  let name = expect_string p in
  expect p LBRACE;

  let health = ref None in
  let img = ref None in
  let movement = ref None in
  let count = ref None in
  let position = ref None in

  while cur p <> RBRACE do
    match cur p with
    | HEALTH ->
        advance p;
        expect p COLON;
        health := Some (parse_expr p)
    | IMG ->
        advance p;
        expect p COLON;
        img := Some (expect_filepath p)
    | MOVEMENT -> movement := Some (parse_movement p)
    | COUNT ->
        advance p;
        expect p COLON;
        count := Some (parse_expr p)
    | POSITION ->
        advance p;
        position := Some (parse_position_pair p)
    | t ->
        parse_error
          (Printf.sprintf "Unexpected field in monster block: %s"
             (token_to_string t))
          (cur_pos p)
  done;
  expect p RBRACE;

  let require field_name = function
    | Some v -> v
    | None ->
        parse_error
          (Printf.sprintf "Monster \"%s\" is missing required field: %s" name
             field_name)
          (cur_pos p)
  in
  {
    m_name = name;
    m_health = require "health" !health;
    m_img = !img;
    m_movement = require "movement" !movement;
    m_count = require "count" !count;
    m_position = !position;
  }

(* ============================================================
   SECTION 11 — ASSIGN STATEMENT
   ============================================================ *)

let parse_assign_target p =
  match cur p with
  | STRING_LIT s -> advance p; TgString s
  | IDENT s -> advance p; TgIdent s
  | t ->
      parse_error
        (Printf.sprintf "Expected assign target, found %s" (token_to_string t))
        (cur_pos p)

let parse_assign_ability p =
  match cur p with
  | STRING_LIT s -> advance p; AbName s
  | IDENT s -> advance p; AbParam s
  | t ->
      parse_error
        (Printf.sprintf "Expected an ability name, found %s" (token_to_string t))
        (cur_pos p)

let parse_assign_stmt p =
  expect p ASSIGN;
  let target = parse_assign_target p in
  expect p ABILITIES;
  expect p LBRACKET;
  let names = ref [ parse_assign_ability p ] in
  while cur p = COMMA do
    advance p;
    names := parse_assign_ability p :: !names
  done;
  expect p RBRACKET;
  { asg_target = target; asg_abilities = List.rev !names }

(* ============================================================
   SECTION 12 — OBSTACLE BLOCK
   ============================================================ *)

let parse_obstacle p =
  expect p OBSTACLE;
  let name = expect_string p in
  expect p LBRACE;
  expect p POSITION;
  let position = parse_position_pair p in

  let size = ref None in
  let img = ref None in
  while cur p <> RBRACE do
    match cur p with
    | SIZE -> (
        advance p;
        expect p COLON;
        match cur p with
        | GRID_DIM_LIT (w, h) ->
            advance p;
            size := Some { width = w; height = h }
        | t ->
            parse_error
              (Printf.sprintf "Expected grid dimension, found %s"
                 (token_to_string t))
              (cur_pos p))
    | IMG ->
        advance p;
        expect p COLON;
        img := Some (expect_filepath p)
    | t ->
        parse_error
          (Printf.sprintf "Unexpected field in obstacle block: %s"
             (token_to_string t))
          (cur_pos p)
  done;
  expect p RBRACE;
  { o_name = name; o_position = position; o_size = !size; o_img = !img }

(* ============================================================
   SECTION 13 — WIN CONDITION BLOCK
   ============================================================ *)

let parse_win_condition p =
  expect p WIN_CONDITION;
  expect p LBRACE;
  let fields = ref [] in
  while cur p <> RBRACE do
    let field =
      match cur p with
      | SURVIVE -> (
          advance p;
          expect p COLON;
          match cur p with
          | DURATION_LIT n -> advance p; WSurvive n
          | t ->
              parse_error
                (Printf.sprintf "Expected a duration, found %s"
                   (token_to_string t))
                (cur_pos p))
      | KILL_MONSTERS ->
          advance p;
          expect p COLON;
          WKillMonsters (parse_condition p)
      | KILL_PLAYERS ->
          advance p;
          expect p COLON;
          WKillPlayers (parse_condition p)
      | ELIMINATION ->
          advance p;
          expect p COLON;
          WElimination (parse_condition p)
      | t ->
          parse_error
            (Printf.sprintf "Unexpected win field: %s" (token_to_string t))
            (cur_pos p)
    in
    fields := field :: !fields
  done;
  expect p RBRACE;
  { w_fields = List.rev !fields }

(* ============================================================
   SECTION 14 — TOP LEVEL PROGRAM
   ============================================================ *)

let starts_stmt = function
  | VAR | IF | LOOP | FN | SPAWN | IDENT _ | STRING_LIT _ -> true
  | _ -> false

let parse_program tokens =
  let p = make_parser tokens in

  let pre = ref [] in
  while cur p <> WORLD && starts_stmt (cur p) do
    pre := parse_stmt p :: !pre
  done;

  let world = parse_world p in

  let players = ref [ parse_player p ] in
  while cur p = PLAYER do
    players := parse_player p :: !players
  done;

  let abilities = ref [] in
  while cur p = ABILITY do
    abilities := parse_ability p :: !abilities
  done;

  let monsters = ref [] in
  while cur p = MONSTER do
    monsters := parse_monster p :: !monsters
  done;

  let assigns = ref [] in
  while cur p = ASSIGN do
    assigns := parse_assign_stmt p :: !assigns
  done;

  let obstacles = ref [] in
  while cur p = OBSTACLE do
    obstacles := parse_obstacle p :: !obstacles
  done;

  let win_condition = parse_win_condition p in

  let post = ref [] in
  while cur p <> EOF do
    post := parse_stmt p :: !post
  done;

  {
    pre_stmts = List.rev !pre;
    prog_world = world;
    players = List.rev !players;
    abilities = List.rev !abilities;
    monsters = List.rev !monsters;
    assigns = List.rev !assigns;
    obstacles = List.rev !obstacles;
    win_condition;
    post_stmts = List.rev !post;
  }

(* ============================================================
   SECTION 15 — ENTRY POINT FROM SOURCE FILE
   ============================================================ *)

let parse_file filename =
  let tokens = load_file filename in
  parse_program tokens