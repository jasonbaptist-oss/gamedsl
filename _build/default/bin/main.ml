open Gamedsl_lib

let () =
  (* 1. Check if the user provided a file *)
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: dune exec bin/main.exe <path/to/game.gdsl>\n";
    exit 1
  end;

  let filename = Sys.argv.(1) in
  Printf.printf "Compiling %s...\n" filename;

  try
    (* 2. Parse the code into an AST *)
    let ast = Parser.parse_file filename in

    (* 3. Run Semantic Analysis to check for logic errors *)
    let check_res = Static_analysis.check_program ast in
    if Static_analysis.has_errors check_res then begin
      Printf.eprintf "COMPILATION FAILED! Semantic errors found:\n";
      List.iter (fun e -> Printf.eprintf "  - %s\n" e) check_res.errors;
      exit 1
    end;

    (* 4. Generate the Python code *)
    let py_code = Codegen.generate ast in
    
    (* 5. Save the generated Python to a file *)
    let out_filename = (Filename.remove_extension filename) ^ ".py" in
    let oc = open_out out_filename in
    output_string oc py_code;
    close_out oc;

    Printf.printf "SUCCESS! Generated %s\n" out_filename;
    Printf.printf "----------------------------------------\n";
    Printf.printf "To play your game, run:\n";
    Printf.printf "  python3 %s\n" out_filename

  with Parser.ParseError (msg, pos) ->
    Printf.eprintf "SYNTAX ERROR at line %d, col %d: %s\n" pos.line pos.col msg;
    exit 1