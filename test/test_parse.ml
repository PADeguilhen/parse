open Expressions

let test_eval_expression () =
  let test_cases = [
    ("1+2", 3.0);
    ("4-2", 2.0);
    ("3*2", 6.0);
    ("6/2", 3.0);
    ("(1+2)*3", 9.0);
    ("4*(2+3)", 20.0);
    ("10/(  5-3)", 5.0);
    ("-3+5", 2.0);
    ("-3*-2", 6.0);
    ("3.5+2.5", 6.0);
    ("(2.5*2)+1", 6.0);
    ("(2+3)*2-1", 9.0);
    ("(4-2)/2", 1.0);
    ("(3+5)/2", 4.0);
    ("(6/3)+2", 4.0);
    ("(1+2)*(3+4)", 21.0);
    ("((1+2)*3)+4", 13.0);
    ("1+(2*3)", 7.0);
    ("1+(2*(3+4))", 15.0);
    ("1+(2*3)+4", 11.0);
    ("1+(2*(3+4))-5", 10.0);
    ("2/3*3", 2.0);
  ] in
  List.iter (fun (input, expected) ->
    let result = parse_expr input in
    if (result = expected) then 
      Printf.printf "Test passed for input: %s\n" input
    else 
      Printf.printf "Test failed for input: %s, expected: %f, got: %f\n" input expected result
  ) test_cases

let () =
  test_eval_expression ()
