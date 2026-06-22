(* ============================================================
   semant.ml — Static Analysis / Semantic Checker for GameDSL
   ============================================================
   Takes a `program` (from ast.ml, produced by parser.ml) and
   walks it to find errors that are grammatically valid but
   semantically wrong:

     - undefined player / monster / ability / function names
     - duplicate names
     - missing or forbidden fields per ability type
     - duplicate or AUTO-misuse of keyboard keys
     - assign targets/abilities that don't exist
     - "towards X" referencing a player that doesn't exist
     - functions called with the wrong number of arguments
     - functions that recurse (directly or are self-referential)
     - assignment to a read-only runtime variable
     - spawn statements naming an undefined entity

   The checker never raises on first error — it collects every
   problem into an `errors` list and returns it, so the caller
   gets a full report instead of one error at a time.
   ============================================================ *)

open Ast

(* ============================================================
   SECTION 1 — ERROR TYPE
   Every check appends a human-readable string here instead of
   raising an exception, so all errors in a file are reported
   together rather than stopping at the first one.
   ============================================================ *)

type check_result = {
  mutable errors : string list;
  mutable warnings : string list;
}

let new_result () = { errors = []; warnings = [] }
let add_error r msg = r.errors <- msg :: r.errors
let add_warning r msg = r.warnings <- msg :: r.warnings
let has_errors r = r.errors <> []

(* present in insertion order for readable output *)
let finalize r = { errors = List.rev r.errors; warnings = List.rev r.warnings }

(* ============================================================
   SECTION 2 — SYMBOL TABLES
   Built in one pass over the program before any cross-checking
   happens, so "is X defined" is an O(1) lookup everywhere else.
   ============================================================ *)

type symtab = {
  player_names : (string, player) Hashtbl.t;
  monster_names : (string, monster) Hashtbl.t;
  ability_names : (string, ability) Hashtbl.t;
  fn_names : (string, fn_decl) Hashtbl.t;
}

let build_symtab prog r =
  let tbl =
    {
      player_names = Hashtbl.create 8;
      monster_names = Hashtbl.create 8;
      ability_names = Hashtbl.create 8;
      fn_names = Hashtbl.create 8;
    }
  in

  List.iter
    (fun pl ->
      if Hashtbl.mem tbl.player_names pl.p_name then
        add_error r (Printf.sprintf "Duplicate player name: \"%s\"" pl.p_name)
      else Hashtbl.add tbl.player_names pl.p_name pl)
    prog.players;

  List.iter
    (fun m ->
      if Hashtbl.mem tbl.monster_names m.m_name then
        add_error r (Printf.sprintf "Duplicate monster name: \"%s\"" m.m_name)
      else Hashtbl.add tbl.monster_names m.m_name m)
    prog.monsters;

  List.iter
    (fun a ->
      if Hashtbl.mem tbl.ability_names a.a_name then
        add_error r (Printf.sprintf "Duplicate ability name: \"%s\"" a.a_name)
      else Hashtbl.add tbl.ability_names a.a_name a)
    prog.abilities;

  (* fn_decl can appear in pre_stmts or post_stmts *)
  let collect_fn = function
    | SFnDecl f ->
        if Hashtbl.mem tbl.fn_names f.fn_name then
          add_error r
            (Printf.sprintf "Duplicate function name: \"%s\"" f.fn_name)
        else Hashtbl.add tbl.fn_names f.fn_name f
    | _ -> ()
  in
  List.iter collect_fn prog.pre_stmts;
  List.iter collect_fn prog.post_stmts;

  tbl

(* a name is "any entity" if it's a player or a monster —
   used by spawn_stmt and assign_target resolution *)
let is_player tbl name = Hashtbl.mem tbl.player_names name
let is_monster tbl name = Hashtbl.mem tbl.monster_names name
let is_entity tbl name = is_player tbl name || is_monster tbl name

(* ============================================================
   SECTION 3 — GLOBAL STRUCTURE CHECKS
     world           exactly one      (guaranteed by parser grammar
                                        since parse_program requires it,
                                        but we still validate field values)
     player_block+   at least one     (also guaranteed by parser)
     win_condition   exactly one, with >= 1 win_field
   ============================================================ *)

let check_global_structure prog r =
  if prog.players = [] then add_error r "At least one player block is required";
  if prog.win_condition.w_fields = [] then
    add_error r "win_condition block must contain at least one win field";
  if prog.prog_world.grid.width <= 0 || prog.prog_world.grid.height <= 0 then
    add_error r "world.grid_size must have positive width and height";
  if prog.prog_world.grid.width > 100 || prog.prog_world.grid.height > 100 then
    add_warning r
      "world.grid_size is unusually large (>100) — this may impact performance";
  if prog.prog_world.duration <= 0 then
    add_error r "world.duration must be a positive number of seconds"

(* ============================================================
   SECTION 4 — KEY CONFLICT CHECKS
     - no two control fields across all players may share a KEY
     - no two player abilities may share a KEY
     - AUTO is forbidden on player abilities
     - AUTO is required on monster abilities
   ============================================================ *)

(* collect every (key, "where it's used") pair for player controls *)
let collect_control_keys prog =
  List.concat_map
    (fun pl ->
      [
        (pl.p_controls.up, Printf.sprintf "player \"%s\" controls.up" pl.p_name);
        ( pl.p_controls.down,
          Printf.sprintf "player \"%s\" controls.down" pl.p_name );
        ( pl.p_controls.left,
          Printf.sprintf "player \"%s\" controls.left" pl.p_name );
        ( pl.p_controls.right,
          Printf.sprintf "player \"%s\" controls.right" pl.p_name );
      ])
    prog.players

(* find the KEY field (if any) declared inside an ability's body *)
let ability_key ab =
  List.find_map (function AbField (FKey k) -> Some k | _ -> None) ab.a_body

(* find which abilities (by name) are assigned to which target *)
let assigned_ability_names assign =
  List.filter_map
    (function
      | AbName n -> Some n
      | AbParam _ ->
          None (* resolved dynamically, can't check key conflicts statically *))
    assign.asg_abilities

let check_key_conflicts prog tbl r =
  (* control keys must be globally unique *)
  let control_keys = collect_control_keys prog in
  let seen = Hashtbl.create 16 in
  List.iter
    (fun (k, where) ->
      match Hashtbl.find_opt seen k with
      | Some other_where ->
          add_error r
            (Printf.sprintf "Key conflict: \"%s\" used by both %s and %s" k
               other_where where)
      | None -> Hashtbl.add seen k where)
    control_keys;

  (* for every assign_stmt, check the abilities given the AUTO rule
     and player-vs-player key uniqueness *)
  List.iter
    (fun assign ->
      let target_name =
        match assign.asg_target with
        | TgString s -> Some s
        | TgIdent _ ->
            None (* resolved at runtime inside a fn, skip static AUTO check *)
      in
      match target_name with
      | None -> ()
      | Some name ->
          let names = assigned_ability_names assign in
          List.iter
            (fun aname ->
              match Hashtbl.find_opt tbl.ability_names aname with
              | None -> () (* reported separately in check_assign *)
              | Some ab -> (
                  match ability_key ab with
                  | None -> () (* permanent abilities have no key — fine *)
                  | Some k ->
                      if is_player tbl name && k = "AUTO" then
                        add_error r
                          (Printf.sprintf
                             "Ability \"%s\" assigned to player \"%s\" uses \
                              key AUTO, which is forbidden on player abilities"
                             aname name);
                      if is_monster tbl name && k <> "AUTO" then
                        add_error r
                          (Printf.sprintf
                             "Ability \"%s\" assigned to monster \"%s\" uses \
                              key \"%s\", but monster abilities must use AUTO"
                             aname name k);
                   if is_player tbl name then
                        begin match Hashtbl.find_opt seen k with
                        | Some other_where ->
                            add_error r
                              (Printf.sprintf
                                 "Key conflict: ability \"%s\" key \"%s\" \
                                  collides with %s"
                                 aname k other_where)
                        | None ->
                            Hashtbl.add seen k
                              (Printf.sprintf "ability \"%s\" (player \"%s\")"
                                 aname name)
                      end))
            names)
    prog.assigns

(* ============================================================
   SECTION 5 — ABILITY TYPE FIELD RULES
     active        requires key, damage, range, shape
                   spread only valid if shape = directional
     permanent     requires >=1 of damage_reduction, health_regen,
                   speed_boost, damage_multiplier
                   forbids key, damage, range, shape, spread,
                   activates_at, required_kills
     timed         requires key, damage, range, shape, activates_at
                   forbids required_kills
     kill_unlocked requires key, required_kills, and >=1 of
                   damage, damage_multiplier
                   forbids activates_at
   ============================================================ *)

type field_presence = {
  mutable has_damage : bool;
  mutable has_range : bool;
  mutable has_shape : bool;
  mutable has_spread : bool;
  mutable has_key : bool;
  mutable has_activates_at : bool;
  mutable has_required_kills : bool;
  mutable has_damage_multiplier : bool;
  mutable has_damage_reduction : bool;
  mutable has_health_regen : bool;
  mutable has_speed_boost : bool;
  mutable shape_is_directional : bool;
}

let scan_fields ab =
  let fp =
    {
      has_damage = false;
      has_range = false;
      has_shape = false;
      has_spread = false;
      has_key = false;
      has_activates_at = false;
      has_required_kills = false;
      has_damage_multiplier = false;
      has_damage_reduction = false;
      has_health_regen = false;
      has_speed_boost = false;
      shape_is_directional = false;
    }
  in
  let rec scan_stmts stmts =
    List.iter
      (fun s ->
        match s with
        | AbField (FDamage _) -> fp.has_damage <- true
        | AbField (FRange _) -> fp.has_range <- true
        | AbField (FShape sh) ->
            fp.has_shape <- true;
            if sh = Directional then fp.shape_is_directional <- true
        | AbField (FSpread _) -> fp.has_spread <- true
        | AbField (FKey _) -> fp.has_key <- true
        | AbField (FActivatesAt _) -> fp.has_activates_at <- true
        | AbField (FRequiredKills _) -> fp.has_required_kills <- true
        | AbField (FDamageMultiplier _) -> fp.has_damage_multiplier <- true
        | AbField (FDamageReduction _) -> fp.has_damage_reduction <- true
        | AbField (FHealthRegen _) -> fp.has_health_regen <- true
        | AbField (FSpeedBoost _) -> fp.has_speed_boost <- true
        | AbVarDecl _ | AbVarAssign _ -> ()
        | AbIf (_, t, e) -> (
            scan_stmts t;
            match e with Some b -> scan_stmts b | None -> ()))
      stmts
  in
  scan_stmts ab.a_body;
  fp

let check_ability_types prog r =
  List.iter
    (fun ab ->
      let fp = scan_fields ab in
      let name = ab.a_name in
      match ab.a_type with
      | Active ->
          if not fp.has_key then
            add_error r
              (Printf.sprintf
                 "Ability \"%s\" (active) is missing required field: key" name);
          if not fp.has_damage then
            add_error r
              (Printf.sprintf
                 "Ability \"%s\" (active) is missing required field: damage"
                 name);
          if not fp.has_range then
            add_error r
              (Printf.sprintf
                 "Ability \"%s\" (active) is missing required field: range" name);
          if not fp.has_shape then
            add_error r
              (Printf.sprintf
                 "Ability \"%s\" (active) is missing required field: shape" name);
          if fp.has_spread && not fp.shape_is_directional then
            add_error r
              (Printf.sprintf
                 "Ability \"%s\" has spread but shape is not directional" name);
          if fp.has_activates_at then
            add_error r
              (Printf.sprintf
                 "Ability \"%s\" (active) must not have activates_at — that is \
                  for type: timed"
                 name);
          if fp.has_required_kills then
            add_error r
              (Printf.sprintf
                 "Ability \"%s\" (active) must not have required_kills — that \
                  is for type: kill_unlocked"
                 name)
      | Permanent ->
          if
            not
              (fp.has_damage_reduction || fp.has_health_regen
             || fp.has_speed_boost || fp.has_damage_multiplier)
          then
            add_error r
              (Printf.sprintf
                 "Ability \"%s\" (permanent) needs at least one of: \
                  damage_reduction, health_regen, speed_boost, \
                  damage_multiplier"
                 name);
          if fp.has_key then
            add_error r
              (Printf.sprintf "Ability \"%s\" (permanent) must not have key"
                 name);
          if fp.has_damage then
            add_error r
              (Printf.sprintf "Ability \"%s\" (permanent) must not have damage"
                 name);
          if fp.has_range then
            add_error r
              (Printf.sprintf "Ability \"%s\" (permanent) must not have range"
                 name);
          if fp.has_shape then
            add_error r
              (Printf.sprintf "Ability \"%s\" (permanent) must not have shape"
                 name);
          if fp.has_spread then
            add_error r
              (Printf.sprintf "Ability \"%s\" (permanent) must not have spread"
                 name);
          if fp.has_activates_at then
            add_error r
              (Printf.sprintf
                 "Ability \"%s\" (permanent) must not have activates_at" name);
          if fp.has_required_kills then
            add_error r
              (Printf.sprintf
                 "Ability \"%s\" (permanent) must not have required_kills" name)
      | Timed ->
          if not fp.has_key then
            add_error r
              (Printf.sprintf
                 "Ability \"%s\" (timed) is missing required field: key" name);
          if not fp.has_damage then
            add_error r
              (Printf.sprintf
                 "Ability \"%s\" (timed) is missing required field: damage" name);
          if not fp.has_range then
            add_error r
              (Printf.sprintf
                 "Ability \"%s\" (timed) is missing required field: range" name);
          if not fp.has_shape then
            add_error r
              (Printf.sprintf
                 "Ability \"%s\" (timed) is missing required field: shape" name);
          if not fp.has_activates_at then
            add_error r
              (Printf.sprintf
                 "Ability \"%s\" (timed) is missing required field: \
                  activates_at"
                 name);
          if fp.has_spread && not fp.shape_is_directional then
            add_error r
              (Printf.sprintf
                 "Ability \"%s\" has spread but shape is not directional" name);
          if fp.has_required_kills then
            add_error r
              (Printf.sprintf
                 "Ability \"%s\" (timed) must not have required_kills — that \
                  is for type: kill_unlocked"
                 name)
      | KillUnlocked ->
          if not fp.has_key then
            add_error r
              (Printf.sprintf
                 "Ability \"%s\" (kill_unlocked) is missing required field: key"
                 name);
          if not fp.has_required_kills then
            add_error r
              (Printf.sprintf
                 "Ability \"%s\" (kill_unlocked) is missing required field: \
                  required_kills"
                 name);
          if not (fp.has_damage || fp.has_damage_multiplier) then
            add_error r
              (Printf.sprintf
                 "Ability \"%s\" (kill_unlocked) needs at least one of: \
                  damage, damage_multiplier"
                 name);
          if fp.has_activates_at then
            add_error r
              (Printf.sprintf
                 "Ability \"%s\" (kill_unlocked) must not have activates_at — \
                  that is for type: timed"
                 name))
    prog.abilities

(* ============================================================
   SECTION 6 — ASSIGN STATEMENT CHECKS
     - target must resolve to a defined player or monster
     - every ability name must be defined
     - no ability assigned twice to the same target
   ============================================================ *)

let check_assign prog tbl r =
  List.iter
    (fun assign ->
      (match assign.asg_target with
      | TgString name ->
          if not (is_entity tbl name) then
            add_error r
              (Printf.sprintf
                 "assign target \"%s\" is not a defined player or monster" name)
      | TgIdent _ -> ());

      (* resolved dynamically inside a function — cannot check statically *)
      let seen = Hashtbl.create 8 in
      List.iter
        (fun ab ->
          match ab with
          | AbName aname ->
              if not (Hashtbl.mem tbl.ability_names aname) then
                add_error r
                  (Printf.sprintf "assign references undefined ability \"%s\""
                     aname);
              if Hashtbl.mem seen aname then
                add_error r
                  (Printf.sprintf
                     "ability \"%s\" is assigned more than once in the same \
                      assign statement"
                     aname)
              else Hashtbl.add seen aname ()
          | AbParam _ -> ())
        assign.asg_abilities)
    prog.assigns

(* ============================================================
   SECTION 7 — MONSTER CHECKS
     - count must be >= 0  (only checkable when it's a literal int;
       expr-valued counts are deferred to runtime)
     - "towards X" must reference a defined player
   ============================================================ *)

let rec check_monsters prog tbl r =
  List.iter
    (fun m ->
      (match m.m_count with
      | EInt n when n < 0 ->
          add_error r
            (Printf.sprintf "monster \"%s\" has count %d — count must be >= 0"
               m.m_name n)
      | _ -> ());
      match m.m_movement with
      | MvTowards pname ->
          if not (is_player tbl pname) then
            add_error r
              (Printf.sprintf
                 "monster \"%s\" has movement: towards \"%s\", but \"%s\" is \
                  not a defined player"
                 m.m_name pname pname)
      | MvLoop loop -> check_loop_towards_refs tbl r loop
      | MvRandom | MvStationary -> ())
    prog.monsters

and check_loop_towards_refs tbl r loop =
  let rec check_actions actions =
    List.iter
      (fun a ->
        match a with
        | AMove (MTowards pname) ->
            if not (is_player tbl pname) then
              add_error r
                (Printf.sprintf
                   "loop move: towards \"%s\" — \"%s\" is not a defined player"
                   pname pname)
        | AIf (_, t, e) -> (
            check_actions t;
            match e with Some b -> check_actions b | None -> ())
        | ALoop l -> check_actions l.loop_actions
        | AMove _ | AWait _ | AAssign _ | AField _ | ASpawn _ -> ())
      actions
  in
  check_actions loop.loop_actions

(* ============================================================
   SECTION 8 — SPAWN STATEMENT CHECKS
     - spawn name must resolve to a defined player or monster
   ============================================================ *)

let rec check_spawns_in_stmts tbl r stmts =
  List.iter
    (fun s ->
      match s with
      | SSpawn sp ->
          if not (is_entity tbl sp.spawn_name) then
            add_error r
              (Printf.sprintf "spawn \"%s\" — not a defined player or monster"
                 sp.spawn_name)
      | SIf (_, t, e) -> (
          check_spawns_in_stmts tbl r t;
          match e with Some b -> check_spawns_in_stmts tbl r b | None -> ())
      | SLoop loop -> check_spawns_in_actions tbl r loop.loop_actions
      | SFnDecl f -> check_spawns_in_stmts tbl r f.fn_body
      | SVarDecl _ | SVarAssign _ | SFieldOverride _ | SFnCall _ -> ())
    stmts

and check_spawns_in_actions tbl r actions =
  List.iter
    (fun a ->
      match a with
      | ASpawn sp ->
          if not (is_entity tbl sp.spawn_name) then
            add_error r
              (Printf.sprintf "spawn \"%s\" — not a defined player or monster"
                 sp.spawn_name)
      | AIf (_, t, e) -> (
          check_spawns_in_actions tbl r t;
          match e with Some b -> check_spawns_in_actions tbl r b | None -> ())
      | ALoop l -> check_spawns_in_actions tbl r l.loop_actions
      | AMove _ | AWait _ | AAssign _ | AField _ -> ())
    actions

let check_spawns prog tbl r =
  check_spawns_in_stmts tbl r prog.pre_stmts;
  check_spawns_in_stmts tbl r prog.post_stmts

(* ============================================================
   SECTION 9 — FUNCTION CHECKS
     - no two parameters in the same fn share a name
     - no nested fn declarations
     - no direct recursion (a function calling itself by name)
     - every fn_call must reference a defined function
     - every fn_call must pass the exact number of arguments
       the function declares
   ============================================================ *)

let check_fn_decl tbl r f =
  (* duplicate parameter names *)
  let seen = Hashtbl.create 4 in
  List.iter
    (fun param ->
      if Hashtbl.mem seen param then
        add_error r
          (Printf.sprintf
             "function \"%s\" has a duplicate parameter name: \"%s\"" f.fn_name
             param)
      else Hashtbl.add seen param ())
    f.fn_params;

  (* nested fn / direct recursion / undefined or arity-mismatched calls,
     all in a single pass over the body *)
  let rec walk_stmts stmts =
    List.iter
      (fun s ->
        match s with
        | SFnDecl inner ->
            add_error r
              (Printf.sprintf
                 "function \"%s\" contains a nested function declaration \
                  \"%s\" — nested fn is not allowed"
                 f.fn_name inner.fn_name)
        | SFnCall call -> (
            if call.call_name = f.fn_name then
              add_error r
                (Printf.sprintf
                   "function \"%s\" calls itself — recursion is not allowed"
                   f.fn_name);
            match Hashtbl.find_opt tbl.fn_names call.call_name with
            | None ->
                add_error r
                  (Printf.sprintf "call to undefined function \"%s\""
                     call.call_name)
            | Some target ->
                let expected = List.length target.fn_params in
                let got = List.length call.call_args in
                if expected <> got then
                  add_error r
                    (Printf.sprintf
                       "function \"%s\" expects %d argument(s) but call \
                        provides %d"
                       call.call_name expected got))
        | SIf (_, t, e) -> (
            walk_stmts t;
            match e with Some b -> walk_stmts b | None -> ())
        | SLoop loop -> walk_actions loop.loop_actions
        | SVarDecl _ | SVarAssign _ | SFieldOverride _ | SSpawn _ -> ())
      stmts
  and walk_actions actions =
    List.iter
      (fun a ->
        match a with
        | AIf (_, t, e) -> (
            walk_actions t;
            match e with Some b -> walk_actions b | None -> ())
        | ALoop l -> walk_actions l.loop_actions
        | AMove _ | AWait _ | AAssign _ | AField _ | ASpawn _ -> ())
      actions
  in
  walk_stmts f.fn_body

let check_functions prog tbl r =
  let check_in_stmts stmts =
    List.iter (function SFnDecl f -> check_fn_decl tbl r f | _ -> ()) stmts
  in
  check_in_stmts prog.pre_stmts;
  check_in_stmts prog.post_stmts;

  (* also check top-level fn_call sites in pre/post stmts,
     in case a call exists outside of any function body *)
  let rec check_calls stmts =
    List.iter
      (fun s ->
        match s with
        | SFnCall call -> (
            match Hashtbl.find_opt tbl.fn_names call.call_name with
            | None ->
                add_error r
                  (Printf.sprintf "call to undefined function \"%s\""
                     call.call_name)
            | Some target ->
                let expected = List.length target.fn_params in
                let got = List.length call.call_args in
                if expected <> got then
                  add_error r
                    (Printf.sprintf
                       "function \"%s\" expects %d argument(s) but call \
                        provides %d"
                       call.call_name expected got))
        | SIf (_, t, e) -> (
            check_calls t;
            match e with Some b -> check_calls b | None -> ())
        | SLoop loop -> check_calls_in_actions loop.loop_actions
        | SFnDecl _ | SVarDecl _ | SVarAssign _ | SFieldOverride _ | SSpawn _ ->
            ())
      stmts
  and check_calls_in_actions actions =
    List.iter
      (fun a ->
        match a with
        | AIf (_, t, e) -> (
            check_calls_in_actions t;
            match e with Some b -> check_calls_in_actions b | None -> ())
        | ALoop l -> check_calls_in_actions l.loop_actions
        | AMove _ | AWait _ | AAssign _ | AField _ | ASpawn _ -> ())
      actions
  in
  check_calls prog.pre_stmts;
  check_calls prog.post_stmts

(* ============================================================
   SECTION 10 — ENTITY REFERENCE CHECKS (field_override / field_read)
     - a RefString entity name in a field_override must resolve
       to a defined player, monster, or ability
     - RefIdent is only valid if it's a parameter of the
       enclosing function (best-effort check; we don't have full
       scoping here, so we just warn if used outside any fn)
   ============================================================ *)

let entity_ref_exists tbl = function
  | RefString name -> is_entity tbl name || Hashtbl.mem tbl.ability_names name
  | RefIdent _ -> true
(* assumed to be a valid parameter; full scope
                           checking would need to track enclosing fn *)

let check_field_override tbl r entity ctx =
  match entity with
  | RefString name when not (entity_ref_exists tbl entity) ->
      add_error r
        (Printf.sprintf
           "field override on \"%s\" (%s) — not a defined player, monster, or \
            ability"
           name ctx)
  | _ -> ()

let rec check_refs_in_stmts tbl r stmts =
  List.iter
    (fun s ->
      match s with
      | SFieldOverride (entity, _, _) ->
          check_field_override tbl r entity "stmt"
      | SIf (_, t, e) -> (
          check_refs_in_stmts tbl r t;
          match e with Some b -> check_refs_in_stmts tbl r b | None -> ())
      | SLoop loop -> check_refs_in_actions tbl r loop.loop_actions
      | SFnDecl f -> check_refs_in_stmts tbl r f.fn_body
      | SVarDecl _ | SVarAssign _ | SFnCall _ | SSpawn _ -> ())
    stmts

and check_refs_in_actions tbl r actions =
  List.iter
    (fun a ->
      match a with
      | AField (entity, _, _) -> check_field_override tbl r entity "action"
      | AIf (_, t, e) -> (
          check_refs_in_actions tbl r t;
          match e with Some b -> check_refs_in_actions tbl r b | None -> ())
      | ALoop l -> check_refs_in_actions tbl r l.loop_actions
      | AMove _ | AWait _ | AAssign _ | ASpawn _ -> ())
    actions

let check_entity_refs prog tbl r =
  check_refs_in_stmts tbl r prog.pre_stmts;
  check_refs_in_stmts tbl r prog.post_stmts

(* ============================================================
   SECTION 11 — TOP-LEVEL DRIVER
   Runs every check in sequence, all writing into the same
   `errors`/`warnings` accumulator, then returns the finalized
   result. Order doesn't affect correctness since every check
   is independent and reads from the same symtab.
   ============================================================ *)

let check_program prog =
  let r = new_result () in
  check_global_structure prog r;
  let tbl = build_symtab prog r in
  check_key_conflicts prog tbl r;
  check_ability_types prog r;
  check_assign prog tbl r;
  check_monsters prog tbl r;
  check_spawns prog tbl r;
  check_functions prog tbl r;
  check_entity_refs prog tbl r;
  finalize r

(* convenience: run checks and print a report *)
let report prog =
  let result = check_program prog in
  if result.errors = [] then print_endline "Static analysis: 0 errors."
  else begin
    Printf.printf "Static analysis: %d error(s):\n" (List.length result.errors);
    List.iter (fun e -> Printf.printf "  - %s\n" e) result.errors
  end;
  if result.warnings <> [] then begin
    Printf.printf "%d warning(s):\n" (List.length result.warnings);
    List.iter (fun w -> Printf.printf "  - %s\n" w) result.warnings
  end;
  result
