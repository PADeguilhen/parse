open Expressions_lib.Expressions

let test_addition () =
  let result = parse_expr "1+2" in
  Alcotest.(check (float 0.001)) "1+2 should equal 3.0" 3.0 result

let test_subtraction () =
  let result = parse_expr "4-2" in
  Alcotest.(check (float 0.001)) "4-2 should equal 2.0" 2.0 result

let test_multiplication () =
  let result = parse_expr "3*2" in
  Alcotest.(check (float 0.001)) "3*2 should equal 6.0" 6.0 result

let test_division () =
  let result = parse_expr "6/2" in
  Alcotest.(check (float 0.001)) "6/2 should equal 3.0" 3.0 result

let test_parentheses_simple () =
  let result = parse_expr "(1+2)*3" in
  Alcotest.(check (float 0.001)) "(1+2)*3 should equal 9.0" 9.0 result

let test_parentheses_complex () =
  let result = parse_expr "4*(2+3)" in
  Alcotest.(check (float 0.001)) "4*(2+3) should equal 20.0" 20.0 result

let test_negative_numbers () =
  let result = parse_expr "-3+5" in
  Alcotest.(check (float 0.001)) "-3+5 should equal 2.0" 2.0 result

let test_negative_multiplication () =
  let result = parse_expr "-3*-2" in
  Alcotest.(check (float 0.001)) "-3*-2 should equal 6.0" 6.0 result

let test_decimal_numbers () =
  let result = parse_expr "3.5+2.5" in
  Alcotest.(check (float 0.001)) "3.5+2.5 should equal 6.0" 6.0 result

let test_nested_parentheses () =
  let result = parse_expr "((1+2)*3)+4" in
  Alcotest.(check (float 0.001)) "((1+2)*3)+4 should equal 13.0" 13.0 result

let test_operator_precedence () =
  let result = parse_expr "2/3*3" in
  Alcotest.(check (float 0.001)) "2/3*3 should equal 2.0" 2.0 result

(* Main test suite *)
let () =
  let open Alcotest in
  run "Expression Parser Tests"
    [
      ( "basic_operations",
        [
          test_case "addition" `Quick test_addition;
          test_case "subtraction" `Quick test_subtraction;
          test_case "multiplication" `Quick test_multiplication;
          test_case "division" `Quick test_division;
        ] );
      ( "parentheses",
        [
          test_case "simple_parentheses" `Quick test_parentheses_simple;
          test_case "complex_parentheses" `Quick test_parentheses_complex;
          test_case "nested_parentheses" `Quick test_nested_parentheses;
        ] );
      ( "special_cases",
        [
          test_case "negative_numbers" `Quick test_negative_numbers;
          test_case "negative_multiplication" `Quick
            test_negative_multiplication;
          test_case "decimal_numbers" `Quick test_decimal_numbers;
          test_case "operator_precedence" `Quick test_operator_precedence;
        ] );
    ]
