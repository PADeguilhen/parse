open Parse_lib.Parse

let test_error_label () =
  let p = fail [ "original" ] <?> "custom error" in
  let result = parser p "hello" in
  match result with
  | Error msg ->
      Alcotest.(check bool)
        "error should contain custom label" true (String.contains msg 'c')
      (* Simple check for custom error *)
  | Ok _ -> Alcotest.fail "should return error"

let test_alternative_first_succeeds () =
  let p = return 42 <|> return 24 in
  let result = parser p "hello" in
  Alcotest.(check (result int string))
    "alternative should return first success" (Ok 42) result

let test_alternative_second_succeeds () =
  let p = fail [ "error" ] <|> return 42 in
  let result = parser p "hello" in
  Alcotest.(check (result int string))
    "alternative should try second on first failure" (Ok 42) result

let () =
  let open Alcotest in
  run "parser"
    [
      ( "control",
        [
          test_case "error_label" `Quick test_error_label;
          test_case "alternative_first" `Quick test_alternative_first_succeeds;
          test_case "alternative_second" `Quick test_alternative_second_succeeds;
        ] );
    ]
