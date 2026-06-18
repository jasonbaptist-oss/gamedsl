(*Tokens*)

type token =
  (* ── Block keywords ──────────────────────────────────────── *)
  | WORLD          (* "world"         *)
  | PLAYER         (* "player"        *)
  | ABILITY        (* "ability"       *)
  | MONSTER        (* "monster"       *)
  | OBSTACLE       (* "obstacle"      *)
  | ASSIGN         (* "assign"        *)
  | WIN_CONDITION  (* "win_condition" *)
 
  (* ── World fields ────────────────────────────────────────── *)
  | GRID_SIZE      (* "grid_size" *)
  | DURATION_KW    (* "duration"  — keyword only, value uses DURATION_LIT *)
 
  (* ── Shared fields ───────────────────────────────────────── *)
  | HEALTH         (* "health"   *)
  | IMG            (* "img"      *)
  | POSITION       (* "position" *)
  | SIZE           (* "size"     *)
  | COUNT          (* "count"    *)
 
  (* ── Player fields ───────────────────────────────────────── *)
  | CONTROLS       (* "controls" *)
  | UP             (* "up"       *)
  | DOWN           (* "down"     *)
  | LEFT           (* "left"     *)
  | RIGHT          (* "right"    *)

  (* ── Boolean literals ────────────────────────────────────── *)
  | TRUE           (* "true"  — used by "active" field on player *)
  | FALSE          (* "false" — used by "active" field on player *)
 
  (* ── Ability fields ──────────────────────────────────────── *)
  | TYPE_KW        (* "type"               *)
  | DAMAGE         (* "damage"             *)
  | RANGE          (* "range"              *)
  | SHAPE          (* "shape"              *)
  | SPREAD         (* "spread"             *)
  | KEY_FIELD      (* "key"                *)
  | ACTIVATES_AT   (* "activates_at"       *)
  | REQUIRED_KILLS (* "required_kills"     *)
  | DAMAGE_MULTIPLIER  (* "damage_multiplier"  *)
  | DAMAGE_REDUCTION   (* "damage_reduction"   *)
  | HEALTH_REGEN   (* "health_regen"       *)
  | SPEED_BOOST    (* "speed_boost"        *)
 
  (* ── Ability types ───────────────────────────────────────── *)
  | ACTIVE         (* "active"        *)
  | PERMANENT      (* "permanent"     *)
  | TIMED          (* "timed"         *)
  | KILL_UNLOCKED  (* "kill_unlocked" *)
 
  (* ── Shape values ────────────────────────────────────────── *)
  | MANHATTAN      (* "manhattan"   *)
  | CHEBYSHEV      (* "chebyshev"   *)
  | DIRECTIONAL    (* "directional" *)
 
  (* ── Monster movement ────────────────────────────────────── *)
  | MOVEMENT       (* "movement"   *)
  | TOWARDS        (* "towards"    *)
  | RANDOM         (* "random"     *)
  | STATIONARY     (* "stationary" *)
 
  (* ── Assign ──────────────────────────────────────────────── *)
  | ABILITIES      (* "abilities" *)

  (* ── Spawn ───────────────────────────────────────────────── *)
  | SPAWN          (* "spawn" — runtime instantiation of a player or monster *)
 
  (* ── Win condition ───────────────────────────────────────── *)
  | SURVIVE        (* "survive"       *)
  | KILL_MONSTERS  (* "kill_monsters" *)
  | KILL_PLAYERS   (* "kill_players"  *)
  | ELIMINATION    (* "elimination"   *)
 
  (* ── Variables and functions ─────────────────────────────── *)
  | VAR            (* "var" *)
  | FN             (* "fn"  *)
 
  (* ── Control flow ────────────────────────────────────────── *)
  | IF             (* "if"   *)
  | ELSE           (* "else" *)
 
  (* ── Loop ────────────────────────────────────────────────── *)
  | LOOP           (* "loop"     *)
  | TIMES          (* "times"    *)
  | INFINITE       (* "infinite" *)
  | ACTIONS        (* "actions"  *)
  | MOVE           (* "move"     *)
  | WAIT           (* "wait"     *)
 
  (* ── Runtime variable functions ──────────────────────────── *)
  | MONSTERS_KILLED  (* "monsters_killed" *)
  | MONSTER_COUNT    (* "monster_count"   *)
  | MONSTER_HEALTH   (* "monster_health"  *)
  | PLAYER_HEALTH    (* "player_health"   *)
  | PLAYERS_KILLED   (* "players_killed"  *)
  | PLAYERS_ALIVE    (* "players_alive"   *)
  | TIME_ELAPSED     (* "time_elapsed"    *)
 
  (* ── Logical operators ───────────────────────────────────── *)
  | AND            (* "and" *)
  | OR             (* "or"  *)
  | NOT            (* "not" *)
 
  (* ── Comparison operators ────────────────────────────────── *)
  | EQ             (* "==" *)
  | NEQ            (* "!=" *)
  | GEQ            (* ">=" *)
  | LEQ            (* "<=" *)
  | GT             (* ">"  *)
  | LT             (* "<"  *)
 
  (* ── Arithmetic operators ────────────────────────────────── *)
  | PLUS           (* "+" *)
  | MINUS          (* "-" *)
  | STAR           (* "*" *)
  | SLASH          (* "/" *)
 
  (* ── Punctuation ─────────────────────────────────────────── *)
  | LBRACE         (* "{"  *)
  | RBRACE         (* "}"  *)
  | LBRACKET       (* "["  *)
  | RBRACKET       (* "]"  *)
  | LPAREN         (* "("  *)
  | RPAREN         (* ")"  *)
  | COLON          (* ":"  *)
  | COMMA          (* ","  *)
  | DOT            (* "."  *)
  | ASSIGN_OP      (* "=" assignment, distinct from EQ "==" *)
  | PERCENT        (* "%"  *)
 
  (* ── Structured literals ─────────────────────────────────── *)
  | INT_LIT      of int       (* 42              *)
  | FLOAT_LIT    of float     (* 3.14            *)
  | GRID_DIM_LIT of int * int (* 20x15           *)
  | DURATION_LIT of int       (* 120s            *)
  | PERCENT_LIT  of int       (* 30%             *)
  | STRING_LIT   of string    (* "Goblin"        *)
  | FILEPATH_LIT of string    (* "assets/g.png"  *)
  | KEY_LIT      of string    (* SPACE, W, AUTO  *)
 
  (* ── Identifier fallback ─────────────────────────────────── *)
  | IDENT        of string    (* any non-keyword identifier *)
 
  (* ── End of file ─────────────────────────────────────────── *)
  | EOF

(*Data types to track the line no and column no*)

type position =
{
  line :int;
  col :int
}

let start_position={line=1;col=1};;

(*Errors*)

exception LexError of string * position
let lex_error msg pos = raise (LexError (msg, pos))

type stream=
{
  src:string;
  mutable pos:int;
  mutable line:int;
  mutable col:int;
}

let make_stream src = { src; pos = 0; line = 1; col = 1 }
let current_pos s= {line=s.line ; col=s.col}
let is_eof s = (s.pos >= String.length (s.src))

(* Peek at current char — '\x00' at EOF *)
let peek s =
  if is_eof s then '\x00' else s.src.[s.pos]

let peek2 s =
  if s.pos + 1 >= String.length s.src then '\x00'
  else s.src.[s.pos + 1]
  
let advance s= if (not (is_eof s)) then 
                (if s.src.[s.pos] ='\n' then
                  (s.line<-s.line+1;
                  s.col<-1;)
                else
                  (s.col<-s.col+1);
                s.pos<-s.pos+1)


let next s=
          let c= peek s  in advance s;c

let rec skip s=
                  if (not (is_eof (s))) 
                    then 
                  (
                    let c= peek s in 
                  if (c = ' ' || c = '\t' || c = '\r' || c = '\n') then (advance s; skip s)
                  else (if c='/' && peek2 s ='/' then 
                            (let rec move () =
                              if (not (is_eof s) &&  peek s <> '\n') then (advance s; move ()) else skip s in move ())
                        else
                            (if c='/' && peek2 s='*' then 
                              (let start_pos= current_pos s in 
                                advance s;
                              (let rec move ()=
                                if( is_eof s) then (lex_error "Unterminated block comment" start_pos)
                                else 
                                (
                                  if( (peek s <>'*' || peek2 s <> '/')) then (advance s; move ()) else (advance s; advance s;skip s) 
                                )
                                in advance s; move ()
                              ))
                            )
                        )
                  )
                  
                                
let keywords = [
  "world",             WORLD;
  "player",            PLAYER;
  "ability",           ABILITY;
  "monster",           MONSTER;
  "obstacle",          OBSTACLE;
  "assign",            ASSIGN;
  "win_condition",     WIN_CONDITION;
  "grid_size",         GRID_SIZE;
  "duration",          DURATION_KW;
  "health",            HEALTH;
  "img",               IMG;
  "position",          POSITION;
  "size",              SIZE;
  "count",             COUNT;
  "controls",          CONTROLS;
  "up",                UP;
  "down",              DOWN;
  "left",              LEFT;
  "right",             RIGHT;
  "true",              TRUE;
  "false",             FALSE;
  "type",              TYPE_KW;
  "damage",            DAMAGE;
  "range",             RANGE;
  "shape",             SHAPE;
  "spread",            SPREAD;
  "key",               KEY_FIELD;
  "activates_at",      ACTIVATES_AT;
  "required_kills",    REQUIRED_KILLS;
  "damage_multiplier", DAMAGE_MULTIPLIER;
  "damage_reduction",  DAMAGE_REDUCTION;
  "health_regen",      HEALTH_REGEN;
  "speed_boost",       SPEED_BOOST;
  "active",            ACTIVE;
  "permanent",         PERMANENT;
  "timed",             TIMED;
  "kill_unlocked",     KILL_UNLOCKED;
  "manhattan",         MANHATTAN;
  "chebyshev",         CHEBYSHEV;
  "directional",       DIRECTIONAL;
  "movement",          MOVEMENT;
  "towards",           TOWARDS;
  "random",            RANDOM;
  "stationary",        STATIONARY;
  "abilities",         ABILITIES;
  "spawn",             SPAWN;
  "survive",           SURVIVE;
  "kill_monsters",     KILL_MONSTERS;
  "kill_players",      KILL_PLAYERS;
  "elimination",       ELIMINATION;
  "var",               VAR;
  "fn",                FN;
  "if",                IF;
  "else",              ELSE;
  "loop",              LOOP;
  "times",             TIMES;
  "infinite",          INFINITE;
  "actions",           ACTIONS;
  "move",              MOVE;
  "wait",              WAIT;
  "monsters_killed",   MONSTERS_KILLED;
  "monster_count",     MONSTER_COUNT;
  "monster_health",    MONSTER_HEALTH;
  "player_health",     PLAYER_HEALTH;
  "players_killed",    PLAYERS_KILLED;
  "players_alive",     PLAYERS_ALIVE;
  "time_elapsed",      TIME_ELAPSED;
  "and",               AND;
  "or",                OR;
  "not",               NOT;
]

let lookup word=
        match List.assoc_opt word keywords with
        | Some b -> b
        | None -> IDENT word

let key_set = [
  "W"; "A"; "S"; "D"; "E"; "Q"; "R"; "F";
  "SPACE"; "ENTER"; "UP"; "DOWN"; "LEFT"; "RIGHT"; "AUTO";
]

let is_key word = List.mem word key_set

let is_digit c  = c >= '0' && c <= '9'
let is_upper c  = c >= 'A' && c <= 'Z'
let is_lower c  = c >= 'a' && c <= 'z'
let is_alpha c  = is_upper c || is_lower c || c = '_'
let is_alnum c  = is_alpha c || is_digit c
let is_filepath_char c =
  is_alnum c || c = '/' || c = '\\' || c = '.' || c = '-'


let lex_number s pos=
  let buf= Buffer.create 8 in 
    let rec finish_digits target_buf=
      if (not (is_eof s) && (is_digit (peek s))) then
          (
            Buffer.add_char target_buf (next s);
            finish_digits target_buf
          )
      in finish_digits buf;
      let int_part= int_of_string(Buffer.contents buf) in 

      if (not (is_eof s) && peek s='.' && is_digit (peek2 s)) then 
        (Buffer.add_char buf (next s);
        finish_digits buf;
        FLOAT_LIT (float_of_string(Buffer.contents buf)))
      else if (not (is_eof s) && peek s='x' && is_digit (peek2 s)) then
        (advance s;
        let buf2= Buffer.create 8 in
        finish_digits buf2;
        if ((Buffer.length buf2)=0) then 
            (
              lex_error "Expected digit after x in grid dimension" pos;
            )
        else let height=int_of_string(Buffer.contents buf2) in GRID_DIM_LIT (int_part,height))
      else if( not (is_eof s) && peek s='s' && (is_eof {s with pos=s.pos+1} || not (is_alnum (peek2 s))) )
        then 
          (advance s;
          DURATION_LIT int_part)
        
      else if(not (is_eof s) && peek s='%')   then
        (advance s;
        PERCENT_LIT int_part)
      else 
        INT_LIT int_part

let lex_string s pos=
        advance s;
        let buf = Buffer.create 16 in 
          let has_path_char = ref false in 
            let rec finish_sentence s = 
              if (is_eof s) then (lex_error "Unterminated string missing closing '\"'" pos)
              else if( not(is_eof s ) && peek s ='\n') then
                (lex_error  "Unterminated string- newline inside quotes " pos)
              else if( not (is_eof s) && peek s<>'"' && (peek s ='/' || peek s='\\' || peek s='.' || peek s='-')) then
                (has_path_char:=true;
                Buffer.add_char buf (peek s);
                advance s;
                finish_sentence s)
              else if (not (is_eof s ) && peek s <> '"') then 
                (Buffer.add_char buf (peek s);
                advance s;
                finish_sentence s)
              else
                ()
                  in  finish_sentence s;
            advance s;
            let content=Buffer.contents buf in 
              if  not !has_path_char then STRING_LIT content else FILEPATH_LIT content

let lex_ident s=
                let buf= Buffer.create 32 in 
                let rec finish_word s= 
                  if(not (is_eof s ) && (is_alnum (peek s)))then( Buffer.add_char buf (next s);
                                                                  finish_word s)
                in finish_word s;
                let word=Buffer.contents buf in 
                  if is_key word then KEY_LIT word else lookup word

let lex_op s pos= 
          let c= next s in 
          match c with 
          | '{'  -> LBRACE
          | '}'  -> RBRACE
          | '['  -> LBRACKET
          | ']'  -> RBRACKET
          | '('  -> LPAREN
          | ')'  -> RPAREN
          | ':'  -> COLON
          | ','  -> COMMA
          | '.'  -> DOT
          | '+'  -> PLUS
          | '-'  -> MINUS
          | '*'  -> STAR
          | '/'  -> SLASH
          | '%'  -> PERCENT
          | '='  -> if peek s = '=' then (advance s; EQ)   else ASSIGN_OP
          | '!'  -> if peek s = '=' then (advance s; NEQ)
                    else lex_error "Unexpected '!' — did you mean '!='" pos
          | '>'  -> if peek s = '=' then (advance s; GEQ)  else GT
          | '<'  -> if peek s = '=' then (advance s; LEQ)  else LT
          | c    -> lex_error
                    (Printf.sprintf "Unexpected character '%c' (ASCII %d)" c (Char.code c))
                    pos

let next_token s=
                    skip s;
                    if (is_eof s) then EOF 
                    else
                      let pos= (current_pos s) in 
                      if(is_digit (peek s)) then (lex_number s pos)
                      else if(is_alnum (peek s)) then (lex_ident s )
                      else if(peek s ='"') then lex_string s pos
                      else lex_op s pos
type located_toke=
{
  token : token;
  pos:position;
}


let tokenize src=
                    let s=make_stream src in
                    let rec loop acc=
                    let tok = next_token s in 
                    let pos  = current_pos s in 
                    let new_acc={token = tok;pos}::acc in
                    if (tok = EOF) then (List.rev new_acc)
                    else (loop new_acc)
                    in  loop []

let token_to_string = function
  | WORLD             -> "WORLD"
  | PLAYER            -> "PLAYER"
  | ABILITY           -> "ABILITY"
  | MONSTER           -> "MONSTER"
  | OBSTACLE          -> "OBSTACLE"
  | ASSIGN            -> "ASSIGN"
  | WIN_CONDITION     -> "WIN_CONDITION"
  | GRID_SIZE         -> "GRID_SIZE"
  | DURATION_KW       -> "DURATION"
  | HEALTH            -> "HEALTH"
  | IMG               -> "IMG"
  | POSITION          -> "POSITION"
  | SIZE              -> "SIZE"
  | COUNT             -> "COUNT"
  | CONTROLS          -> "CONTROLS"
  | UP                -> "UP"
  | DOWN              -> "DOWN"
  | LEFT              -> "LEFT"
  | RIGHT             -> "RIGHT"
  | TRUE              -> "TRUE"
  | FALSE             -> "FALSE"
  | TYPE_KW           -> "TYPE"
  | DAMAGE            -> "DAMAGE"
  | RANGE             -> "RANGE"
  | SHAPE             -> "SHAPE"
  | SPREAD            -> "SPREAD"
  | KEY_FIELD         -> "KEY"
  | ACTIVATES_AT      -> "ACTIVATES_AT"
  | REQUIRED_KILLS    -> "REQUIRED_KILLS"
  | DAMAGE_MULTIPLIER -> "DAMAGE_MULTIPLIER"
  | DAMAGE_REDUCTION  -> "DAMAGE_REDUCTION"
  | HEALTH_REGEN      -> "HEALTH_REGEN"
  | SPEED_BOOST       -> "SPEED_BOOST"
  | ACTIVE            -> "ACTIVE"
  | PERMANENT         -> "PERMANENT"
  | TIMED             -> "TIMED"
  | KILL_UNLOCKED     -> "KILL_UNLOCKED"
  | MANHATTAN         -> "MANHATTAN"
  | CHEBYSHEV         -> "CHEBYSHEV"
  | DIRECTIONAL       -> "DIRECTIONAL"
  | MOVEMENT          -> "MOVEMENT"
  | TOWARDS           -> "TOWARDS"
  | RANDOM            -> "RANDOM"
  | STATIONARY        -> "STATIONARY"
  | ABILITIES         -> "ABILITIES"
  | SPAWN             -> "SPAWN"
  | SURVIVE           -> "SURVIVE"
  | KILL_MONSTERS     -> "KILL_MONSTERS"
  | KILL_PLAYERS      -> "KILL_PLAYERS"
  | ELIMINATION       -> "ELIMINATION"
  | VAR               -> "VAR"
  | FN                -> "FN"
  | IF                -> "IF"
  | ELSE              -> "ELSE"
  | LOOP              -> "LOOP"
  | TIMES             -> "TIMES"
  | INFINITE          -> "INFINITE"
  | ACTIONS           -> "ACTIONS"
  | MOVE              -> "MOVE"
  | WAIT              -> "WAIT"
  | MONSTERS_KILLED   -> "MONSTERS_KILLED"
  | MONSTER_COUNT     -> "MONSTER_COUNT"
  | MONSTER_HEALTH    -> "MONSTER_HEALTH"
  | PLAYER_HEALTH     -> "PLAYER_HEALTH"
  | PLAYERS_KILLED    -> "PLAYERS_KILLED"
  | PLAYERS_ALIVE     -> "PLAYERS_ALIVE"
  | TIME_ELAPSED      -> "TIME_ELAPSED"
  | AND               -> "AND"
  | OR                -> "OR"
  | NOT               -> "NOT"
  | EQ                -> "EQ(==)"
  | NEQ               -> "NEQ(!=)"
  | GEQ               -> "GEQ(>=)"
  | LEQ               -> "LEQ(<=)"
  | GT                -> "GT(>)"
  | LT                -> "LT(<)"
  | PLUS              -> "PLUS(+)"
  | MINUS             -> "MINUS(-)"
  | STAR              -> "STAR(*)"
  | SLASH             -> "SLASH(/)"
  | LBRACE            -> "LBRACE({)"
  | RBRACE            -> "RBRACE(})"
  | LBRACKET          -> "LBRACKET([)"
  | RBRACKET          -> "RBRACKET(])"
  | LPAREN            -> "LPAREN(()"
  | RPAREN            -> "RPAREN())"
  | COLON             -> "COLON(:)"
  | COMMA             -> "COMMA(,)"
  | DOT               -> "DOT(.)"
  | ASSIGN_OP         -> "ASSIGN_OP(=)"
  | PERCENT           -> "PERCENT(%)"
  | INT_LIT n         -> Printf.sprintf "INT(%d)" n
  | FLOAT_LIT f       -> Printf.sprintf "FLOAT(%g)" f
  | GRID_DIM_LIT(w,h) -> Printf.sprintf "GRID_DIM(%dx%d)" w h
  | DURATION_LIT n    -> Printf.sprintf "DURATION(%ds)" n
  | PERCENT_LIT n     -> Printf.sprintf "PERCENT_LIT(%d%%)" n
  | STRING_LIT s      -> Printf.sprintf "STRING(\"%s\")" s
  | FILEPATH_LIT s    -> Printf.sprintf "FILEPATH(\"%s\")" s
  | KEY_LIT k         -> Printf.sprintf "KEY(%s)" k
  | IDENT s           -> Printf.sprintf "IDENT(%s)" s
  | EOF               -> "EOF"


let print_tokens tokens =
  List.iter (fun { token; pos } ->
    Printf.printf "  [%3d:%2d]  %s\n"
      pos.line pos.col
      (token_to_string token)
  ) tokens

let load_file filename=
    let ic = open_in filename in
    let len = in_channel_length ic in
    let src= Bytes.create len in 
    really_input ic src 0 len;
    close_in ic;
    tokenize (Bytes.to_string src)