(* ============================================================
   ast.ml — Abstract Syntax Tree for GameDSL
   ============================================================
   This file defines the exact data structures that the parser
   builds. These types represent the "meaning" of the code
   after the raw text has been analyzed.
   ============================================================ *)

(* ─── Basic Building Blocks ────────────────────────────────── *)

type binop = Add | Sub | Mul | Div
type cmp_op = Eq | Neq | Geq | Leq | Gt | Lt
type entity_ref = RefString of string | RefIdent of string

type runtime_fn =
  | MonstersKilled
  | MonsterCount
  | MonsterHealth
  | PlayerHealth
  | PlayersKilled
  | PlayersAlive
  | TimeElapsed

type call_arg = ArgString of string | ArgIdent of string
type grid_dim = { width : int; height : int }

(* ─── Mutually Recursive Core Types ────────────────────────── *)

type expr =
  | EInt of int
  | EFloat of float
  | EVar of string
  | EString of string (* <-- Add this line! *)
  | EBinop of binop * expr * expr
  | ERuntimeCall of runtime_fn * call_arg option
  | EFieldRead of entity_ref * string list

and cond =
  | COr of cond * cond
  | CAnd of cond * cond
  | CNot of cond
  | CCmp of expr * cmp_op * expr

and fn_decl = { fn_name : string; fn_params : string list; fn_body : stmt list }
and fn_call = { call_name : string; call_args : expr list }

and stmt =
  | SVarDecl of string * expr
  | SVarAssign of string * expr
  | SFieldOverride of entity_ref * string list * expr
  | SIf of cond * stmt list * stmt list option
  | SLoop of loop_stmt
  | SFnDecl of fn_decl
  | SFnCall of fn_call
  | SSpawn of spawn_stmt

and loop_count = LFinite of int | LInfinite | LCondition of cond
and loop_stmt = { loop_times : loop_count; loop_actions : action list }

and action =
  | AMove of move_val
  | AWait of int
  | AIf of cond * action list * action list option
  | AAssign of string * expr
  | AField of entity_ref * string list * expr
  | ALoop of loop_stmt
  | ASpawn of spawn_stmt
  | AFnCall of fn_call

and move_val = MUp | MDown | MLeft | MRight | MRandom | MTowards of string
and spawn_stmt = { spawn_name : string; spawn_position : (expr * expr) option }

and ability_stmt =
  | AbField of ability_field
  | AbVarDecl of string * expr
  | AbVarAssign of string * expr
  | AbIf of cond * ability_stmt list * ability_stmt list option

and ability_field =
  | FDamage of expr
  | FRange of expr
  | FShape of shape_val
  | FSpread of expr
  | FKey of string
  | FActivatesAt of int
  | FRequiredKills of expr
  | FDamageMultiplier of expr
  | FDamageReduction of int
  | FHealthRegen of expr
  | FSpeedBoost of expr

and shape_val = Manhattan | Chebyshev | Directional

(* ─── Block Definitions ────────────────────────────────────── *)

type world_block = { grid : grid_dim; duration : int }
type controls = { up : string; down : string; left : string; right : string }

type player = {
  p_name : string;
  p_health : expr;
  p_img : string option;
  p_active : bool;
  p_position : (expr * expr) option;
  p_controls : controls;
}

type ability_type = Active | Permanent | Timed | KillUnlocked

type ability = {
  a_name : string;
  a_type : ability_type;
  a_img : string option;
  a_body : ability_stmt list;
}

type movement_val =
  | MvTowards of string
  | MvRandom
  | MvStationary
  | MvLoop of loop_stmt

type monster = {
  m_name : string;
  m_health : expr;
  m_img : string option;
  m_movement : movement_val;
  m_count : expr;
  m_position : (expr * expr) option;
}

type assign_target = TgString of string | TgIdent of string
type assign_ability = AbName of string | AbParam of string

type assign_stmt = {
  asg_target : assign_target;
  asg_abilities : assign_ability list;
}

type obstacle_block = {
  o_name : string;
  o_position : expr * expr;
  o_size : grid_dim option;
  o_img : string option;
}

type win_field =
  | WSurvive of int
  | WKillMonsters of cond
  | WKillPlayers of cond
  | WElimination of cond

type win_condition_block = { w_fields : win_field list }

(* ─── Top-Level Program ────────────────────────────────────── *)

type program = {
  pre_stmts : stmt list;
  prog_world : world_block;
  players : player list;
  abilities : ability list;
  monsters : monster list;
  assigns : assign_stmt list;
  obstacles : obstacle_block list;
  win_condition : win_condition_block;
  post_stmts : stmt list;
}
