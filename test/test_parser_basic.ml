open Parse_lib.Parse

let test_return () =
  let p = return 42 in
  let result = parser p "hello" in
  Alcotest.(check (result int string)) "return should succeed" (Ok 42) result

let test_fail () =
  let p = fail [ "error message" ] in
  let result = parser p "hello" in
  match result with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "fail should return Error"

let () =
  let open Alcotest in
  run "parser"
    [
      ( "basic",
        [
          test_case "return" `Quick test_return;
          test_case "fail" `Quick test_fail;
          (* test_case "parser incr" `Quick test_parser_incr; *)
        ] );
    ]
