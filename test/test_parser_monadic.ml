open Parse_lib.Parse

let test_bind () =
  let p = return 5 >>= fun x -> return (x * 2) in
  let result = parser p "hello" in
  Alcotest.(check (result int string))
    "bind should chain computations" (Ok 10) result

let test_map () =
  let p = return 5 >>| fun x -> x * 2 in
  let result = parser p "hello" in
  Alcotest.(check (result int string))
    "map should transform result" (Ok 10) result

let test_fmap () =
  let p = (fun x -> x * 2) <$> return 5 in
  let result = parser p "hello" in
  Alcotest.(check (result int string))
    "fmap should transform result" (Ok 10) result

let test_apply () =
  let f = return (fun x -> x * 2) in
  let x = return 5 in
  let p = f <*> x in
  let result = parser p "hello" in
  Alcotest.(check (result int string)) "apply should work" (Ok 10) result

let () =
  let open Alcotest in
  run "parser"
    [
      ( "monads",
        [
          test_case "bind" `Quick test_bind;
          test_case "map" `Quick test_map;
          test_case "fmap" `Quick test_fmap;
          test_case "apply" `Quick test_apply;
        ] );
    ]
