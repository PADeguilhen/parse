open Parse_lib.Parse

let test_sequence_right () =
  let p = return 1 *> return 2 in
  let result = parser p "hello" in
  Alcotest.(check (result int string))
    "sequence right should return second result" (Ok 2) result

let test_sequence_left () =
  let p = return 1 <* return 2 in
  let result = parser p "hello" in
  Alcotest.(check (result int string))
    "sequence left should return first result" (Ok 1) result

let () =
  let open Alcotest in
  run "parser"
    [
      ( "sequencing",
        [
          test_case "sequence_right" `Quick test_sequence_right;
          test_case "sequence_left" `Quick test_sequence_left;
        ] );
    ]
