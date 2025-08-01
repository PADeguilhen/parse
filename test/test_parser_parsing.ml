open Parse_lib.Parse

let test_char incr () =
  let p = if incr then char 'h' else char 'o' in
  let result = parser ~incr:incr p "hello" in
  Alcotest.(check (result char string))
    "char should parse matching character" (if incr then Ok 'h' else Ok 'o') result;

  let result2 = parser ~incr:incr p "world" in
  match result2 with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "char should fail on non-matching character"

let test_string incr () =
  let p = if incr then string "hello" else string "world" in
  let result = parser ~incr:incr p "hello world" in
  Alcotest.(check (result string string))
    "string should parse matching string" (if incr then Ok "hello" else Ok "world") result;

  let result2 = parser ~incr:incr p "hi monde" in
  match result2 with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "string should fail on non-matching string"

let test_take_while incr () =
  let p = take_while (function 'a' .. 'z' -> true | _ -> false) in
  let result = parser ~incr:incr p "hello123hi" in
  Alcotest.(check (result string string))
    "take_while should consume matching chars" (if incr then Ok "hello" else Ok "hi") result

let test_take_while1 incr () =
  let p = take_while1 (function 'a' .. 'z' -> true | _ -> false) in
  let result = parser ~incr:incr p "hello123hi" in
  Alcotest.(check (result string string))
    "take_while1 should consume matching chars" (if incr then Ok "hello" else Ok "hi") result;

  let result2 = parser ~incr:incr p "123hello1" in
  match result2 with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "take_while1 should fail if no chars match"

let test_peek_char incr () =
  let p = peek_char in
  let result = parser ~incr:incr p "hello" in
  Alcotest.(check (result (option char) string))
    "peek_char should return first char" (Ok (if incr then Some 'h' else Some 'o')) result;

  let result2 = parser ~incr:incr p "" in
  Alcotest.(check (result (option char) string))
    "peek_char should return None for empty" (Ok None) result2

let test_peek_string incr () =
  let p = peek_string 3 in
  let result = parser ~incr:incr p "hello" in
  Alcotest.(check (result string string))
    "peek_string should return prefix" (if incr then Ok "hel" else Ok "llo") result

let test_advance incr () =
  let p = advance 2 *> peek_char in
  let result = parser ~incr:incr p "hello" in
  Alcotest.(check (result (option char) string))
    "advance should move position" (Ok (Some 'l')) result

(* let test_pos () =
  let p = pos in
  let result = parser p "hello" in
  Alcotest.(check (result int string)) "pos should return current position" (Ok 0) result;
  
  let p2 = advance 3 *> pos in
  let result2 = parser p2 "hello" in
  Alcotest.(check (result int string)) "pos after advance should be updated" (Ok 3) result2 *)

let () =
  let open Alcotest in
  run "parser"
    [
      ( "parsing forward",
        [
          test_case "char" `Quick (test_char true);
          test_case "string" `Quick (test_string true);
          test_case "take_while" `Quick (test_take_while true);
          test_case "take_while1" `Quick (test_take_while1 true);
          test_case "peek_char" `Quick (test_peek_char true);
          test_case "peek_string" `Quick (test_peek_string true);
          test_case "advance" `Quick (test_advance true);
          (* test_case "pos" `Quick test_pos; *)
        ] );
      ( "parsing reverse",
        [
          test_case "char" `Quick (test_char false);
          test_case "string" `Quick (test_string false);
          test_case "take_while" `Quick (test_take_while false);
          test_case "take_while1" `Quick (test_take_while1 false);
          test_case "peek_char" `Quick (test_peek_char false);
          test_case "peek_string" `Quick (test_peek_string false);
          test_case "advance" `Quick (test_advance false);
          (* test_case "pos" `Quick test_pos; *)
        ] );
    ]
