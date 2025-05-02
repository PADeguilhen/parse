open Parse

let is_digit = function 
  | '0'..'9' -> true 
  | _ -> false

let is_alphanum = function
  | '0'..'9' | 'a'..'z' | 'A'..'Z' -> true
  | _ -> false

let dot =
  peek_char
  >>= function
  | Some '.' -> advance 1 >>| fun () -> true
  | _ -> return false

let whitespace = take_while (fun c -> c = ' ' || c = '\t' || c = '\n')

type expr = 
  | Add of term * expr
  | Sub of term * expr
  | T   of term
and term = 
  | Mult of term * factor
  | Div  of term * factor
  | F    of factor
and factor =
  | Neg of sign
  | Pos of sign
and sign =
  | Parentheses of expr
  | Variable    of string
  | Literal     of float

let sign =
  fix (
    fun sign ->
      peek_char >>= fun s ->
        match s with
        | Some '-' -> advance 1 >>= fun () -> sign >>| fun v -> not v
        | Some '+' -> advance 1 >>= fun () -> sign
        | _ -> return false
  )

let unsignedLit =
  take_while1 is_digit
  >>= fun whole ->
  dot
  >>= function
  | false ->
      return (Literal(float_of_string whole))
  | true ->
      take_while is_digit >>= fun part ->
      return (Literal(float_of_string (whole ^ "." ^ part) ))

let unsignedVar =
  take_while1 is_alphanum >>| fun v -> Variable(v)

let bracketed l p r = 
  char l *> p <* char r <?> (Printf.sprintf "expected closing %c" r)

let factor expr = 
  ((whitespace *> sign) >>= fun s ->
    unsignedLit <|> unsignedVar <|> ((bracketed '(' (expr <?> "expected expression inside parentheses") ')') >>| fun e -> Parentheses e) >>| fun v ->
      if s then Neg(v) else Pos(v)
  )

let term expr =
  factor expr <* whitespace >>= fun f ->

  let rec rest acc =
    peek_char >>= function
    | Some '*' -> 
        char '*' *> whitespace *> factor expr >>= fun next ->
        rest (Mult(acc, next))
    | Some '/' -> 
        char '/' *> whitespace *> factor expr >>= fun next ->
        rest (Div(acc, next))
    | _ -> 
        return acc
  in
  rest (F f)

let expr = 
  fix (fun expr ->
    term expr <* whitespace >>= fun t ->
    peek_char >>= 
      function 
      | Some '+' -> advance 1 >>= fun () -> expr >>| fun e -> Add(t, e)
      | Some '-' -> advance 1 >>= fun () -> expr >>| fun e -> Sub(t, e)
      | _ -> return (T t)
  )

let rec eval =
  function
  | Add (t, e) -> term t +. eval e
  | Sub (t, e) -> term t -. eval e
  | T t -> term t
and term =
  function
  | Mult(t, f) -> term t *. factor f
  | Div(t, f) -> term t /. factor f
  | F f -> factor f
and factor =
  function
  | Neg s -> -. (sign s)
  | Pos s -> sign s
and sign = 
  function
  | Parentheses e -> eval e
  | Variable _v -> 0.
  | Literal v -> v

let rec show_error msgs = 
  match msgs with | x :: xs -> print_string x; print_newline (); show_error xs | [] -> ()

let expression str = 
  match (parse (expr <?> "expected expression") str) with
  | Done (_pos, e) -> Printf.printf "Success: %f\n" (eval e)
  | Fail (pos, str) -> Printf.printf "Failure on caracter %d\n" pos; show_error str
  | _ -> print_string "something went wrong :s\n"
