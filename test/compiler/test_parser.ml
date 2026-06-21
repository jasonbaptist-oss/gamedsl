open Gamedsl_lib.Parser

let () =
  let files = [ "test_11.gdsl"; "test_12.gdsl"; "test_13.gdsl";"test_14.gdsl"] in

  List.iter
    (fun file ->
      Printf.printf "Parsing %s... " file;
      try
        let _ast = parse_file ("test/dsl_programs/" ^ file) in
        Printf.printf "SUCCESS!\n"
      with ParseError (msg, pos) ->
        Printf.printf "FAILED at line %d, col %d: %s\n" pos.line pos.col msg)
    files
