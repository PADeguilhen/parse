open Parse_lib.Parse

let test_char () =
  let p = char 'h' in
  let result = parser p "hello" in
  Alcotest.(check (result char string))
    "char should parse matching character" (Ok 'h') result;

  let result2 = parser p "world" in
  match result2 with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "char should fail on non-matching character"

let test_string () =
  let p = string "hello" in
  let result = parser p "hello world" in
  Alcotest.(check (result string string))
    "string should parse matching string" (Ok "hello") result;

  let result2 = parser p "hi world" in
  match result2 with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "string should fail on non-matching string"

let test_take_while () =
  let p = take_while (function 'a' .. 'z' -> true | _ -> false) in
  let result = parser p "hello123" in
  Alcotest.(check (result string string))
    "take_while should consume matching chars" (Ok "hello") result

let test_take_while1 () =
  let p = take_while1 (function 'a' .. 'z' -> true | _ -> false) in
  let result = parser p "hello123" in
  Alcotest.(check (result string string))
    "take_while1 should consume matching chars" (Ok "hello") result;

  let result2 = parser p "123hello" in
  match result2 with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "take_while1 should fail if no chars match"

let test_peek_char () =
  let p = peek_char in
  let result = parser p "hello" in
  Alcotest.(check (result (option char) string))
    "peek_char should return first char" (Ok (Some 'h')) result;

  let result2 = parser p "" in
  Alcotest.(check (result (option char) string))
    "peek_char should return None for empty" (Ok None) result2

let test_peek_string () =
  let p = peek_string 3 in
  let result = parser p "hello" in
  Alcotest.(check (result string string))
    "peek_string should return prefix" (Ok "hel") result;

  let result2 = parser p "hi" in
  Alcotest.(check (result string string))
    "peek_string should return available chars" (Ok "hi") result2

let test_advance () =
  let p = advance 2 *> peek_char in
  let result = parser p "hello" in
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
      ( "lifting",
        [
          test_case "char" `Quick test_char;
          test_case "string" `Quick test_string;
          test_case "take_while" `Quick test_take_while;
          test_case "take_while1" `Quick test_take_while1;
          test_case "peek_char" `Quick test_peek_char;
          test_case "peek_string" `Quick test_peek_string;
          test_case "advance" `Quick test_advance;
          (* test_case "pos" `Quick test_pos; *)
        ] );
    ]
