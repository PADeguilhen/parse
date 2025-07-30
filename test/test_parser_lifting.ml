open Parse_lib.Parse

let test_lift () =
  let p = lift (return 5) (fun x -> x * 2) in
  let result = parser p "hello" in
  Alcotest.(check (result int string))
    "lift should transform result" (Ok 10) result

let test_lift2 () =
  let p = lift2 ( + ) (return 3) (return 4) in
  let result = parser p "hello" in
  Alcotest.(check (result int string))
    "lift2 should combine two parsers" (Ok 7) result

let test_lift3 () =
  let add3 x y z = x + y + z in
  let p = lift3 add3 (return 1) (return 2) (return 3) in
  let result = parser p "hello" in
  Alcotest.(check (result int string))
    "lift3 should combine three parsers" (Ok 6) result

let test_lift4 () =
  let add4 x y z w = x + y + z + w in
  let p = lift4 add4 (return 1) (return 2) (return 3) (return 4) in
  let result = parser p "hello" in
  Alcotest.(check (result int string))
    "lift4 should combine four parsers" (Ok 10) result

let () =
  let open Alcotest in
  run "parser"
    [
      ( "lifting",
        [
          test_case "lift" `Quick test_lift;
          test_case "lift2" `Quick test_lift2;
          test_case "lift3" `Quick test_lift3;
          test_case "lift4" `Quick test_lift4;
        ] );
    ]
