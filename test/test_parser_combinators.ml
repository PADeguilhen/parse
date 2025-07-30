open Parse_lib.Parse

let test_many () =
  let p = many (char 'a') in
  let result = parser p "aaabbb" in
  Alcotest.(check (result (list char) string))
    "many should parse repeated elements"
    (Ok [ 'a'; 'a'; 'a' ])
    result;

  let result2 = parser p "bbb" in
  Alcotest.(check (result (list char) string))
    "many should succeed with empty list" (Ok []) result2

let test_sep_by1 () =
  let p = sep_by1 (char ',') (char 'a') in
  let result = parser p "a,a,a" in
  Alcotest.(check (result (list char) string))
    "sep_by1 should parse separated elements"
    (Ok [ 'a'; 'a'; 'a' ])
    result;

  let result2 = parser p "b" in
  match result2 with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "sep_by1 should fail with no elements"

let test_sep_by () =
  let p = sep_by (char ',') (char 'a') in
  let result = parser p "a,a,a" in
  Alcotest.(check (result (list char) string))
    "sep_by should parse separated elements"
    (Ok [ 'a'; 'a'; 'a' ])
    result;

  let result2 = parser p "b" in
  Alcotest.(check (result (list char) string))
    "sep_by should succeed with empty list" (Ok []) result2

let () =
  let open Alcotest in
  run "parser"
    [
      ( "combinators",
        [
          test_case "many" `Quick test_many;
          test_case "sep_by1" `Quick test_sep_by1;
          test_case "sep_by" `Quick test_sep_by;
        ] );
    ]
