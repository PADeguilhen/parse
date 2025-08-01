open Parse_lib.Parse

let test_return incr () =
  let p = return 42 in
  let result = parser ~incr:incr p "hello" in
  Alcotest.(check (result int string)) "return should succeed" (Ok 42) result

let test_fail incr () =
  let p = fail [ "error message" ] in
  let result = parser ~incr:incr p "hello" in
  match result with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "fail should return Error"

let () =
  let open Alcotest in
  run "parser"
    [
      ( "basic forward",
        [
          test_case "return" `Quick (test_return true);
          test_case "fail" `Quick (test_fail true);
          (* test_case "parser incr" `Quick test_parser_incr; *)
        ] );
  
      ( "basic reverse",
        [
          test_case "return" `Quick (test_return false);
          test_case "fail" `Quick (test_fail false);
          (* test_case "parser incr" `Quick test_parser_incr; *)
        ] );
    ]
