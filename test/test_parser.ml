open Parse_lib.Parse

let test_fix () =
  (* Simple test for fix - parsing balanced parentheses *)
  let p =
    fix (fun balanced ->
        char '(' *> balanced <* char ')' >>| (fun _ -> "()") <|> return "")
  in
  let result = parser p "(())" in
  match result with
  | Ok _ -> ()
  | Error _ -> Alcotest.fail "fix should handle recursive parsing"

let test_fix_lazy () =
  (* Test fix_lazy with max_steps *)
  let p = fix_lazy ~max_steps:10 (fun p -> return "lazy" <|> p) in
  let result = parser p "test" in
  Alcotest.(check (result string string))
    "fix_lazy should work" (Ok "lazy") result

let test_parser_incr () =
  let p = return 42 in
  let result = parser ~incr:true p "hello" in
  Alcotest.(check (result int string))
    "parser with incr should work" (Ok 42) result;

  let result2 = parser ~incr:false p "hello" in
  Alcotest.(check (result int string))
    "parser without incr should work" (Ok 42) result2

let () =
  let open Alcotest in
  run "Parser Tests"
    [ (* "fixpoint", [
      test_case "fix" `Quick test_fix;
      test_case "fix_lazy" `Quick test_fix_lazy;
    ]; *) ]
