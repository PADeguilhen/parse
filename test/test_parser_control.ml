open Parse_lib.Parse

let test_error_label incr () =
  let p = fail [ "original" ] <?> "custom error" in
  let result = parser ~incr:incr p "hello" in
  match result with
  | Error msg ->
      Alcotest.(check bool)
        "error should contain custom label" true (String.contains msg 'c')
      (* Simple check for custom error *)
  | Ok _ -> Alcotest.fail "should return error"

let test_alternative_first_succeeds incr () =
  let p = return 42 <|> return 24 in
  let result = parser ~incr:incr p "hello" in
  Alcotest.(check (result int string))
    "alternative should return first success" (Ok 42) result

let test_alternative_second_succeeds incr () =
  let p = fail [ "error" ] <|> return 42 in
  let result = parser ~incr:incr p "hello" in
  Alcotest.(check (result int string))
    "alternative should try second on first failure" (Ok 42) result

let () =
  let open Alcotest in
  run "parser"
    [
      ( "control forward",
        [
          test_case "error_label" `Quick (test_error_label true);
          test_case "alternative_first" `Quick (test_alternative_first_succeeds true);
          test_case "alternative_second" `Quick (test_alternative_second_succeeds true);
        ] );
      ( "control reverse",
        [
          test_case "error_label" `Quick (test_error_label false);
          test_case "alternative_first" `Quick (test_alternative_first_succeeds false);
          test_case "alternative_second" `Quick (test_alternative_second_succeeds false);
        ] );
    ]
