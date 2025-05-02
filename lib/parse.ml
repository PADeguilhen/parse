module State = struct
  type 'a t = 
    | Done of int * 'a
    | Fail of int * string list
    | Lazy of 'a t Lazy.t
end

type 'a with_input = string -> int -> 'a
(*
success takes an input string, input position, returns an ast
failure takes the same but returns an error message
*)
type ('a, 'r) success = ('a -> 'r State.t) with_input

type 'r failure = (string list -> 'r State.t) with_input

type 'a t = 
{
  run: 'r. (('a, 'r) success -> 'r failure -> 'r State.t) with_input
}
  
let fail_k _inp pos msg =
  State.Fail(pos, msg)

let succ_k _inp pos a = 
  State.Done(pos, a)

let rec from_state s = match s with 
  | State.Done(pos, ast) -> State.Done(pos, ast)
  | State.Fail(pos, msg) -> State.Fail(pos, msg)
  | State.Lazy(x) -> from_state (Lazy.force x)

let parse p input = from_state (p.run input 0 succ_k fail_k)

let return v = 
{
  run = fun input pos succ _fail -> succ input pos v
}

let fail msg = 
{
  run = fun input pos _succ fail -> fail input pos msg
}

let ( >>= ) p f  = 
{
  run = fun inp pos succ fail -> 
    let succ' inp' pos' v = (f v).run inp' pos' succ fail in
    p.run inp pos succ' fail
}

let (>>|) m f =
{
  run = fun inp pos succ fail ->
    let succ' inp' pos' v  = succ inp' pos' (f v) in
    m.run inp pos succ' fail
}

let (<$>) f m = m >>| f

let (<*>) f m = f >>= fun f -> m >>| f

let lift  = (>>|)
let lift2 f m1 m2       = f <$> m1 <*> m2
let lift3 f m1 m2 m3    = f <$> m1 <*> m2 <*> m3
let lift4 f m1 m2 m3 m4 = f <$> m1 <*> m2 <*> m3 <*> m4

let ( *>) a b =
{
  run = fun inp pos succ fail ->
    let succ' inp' pos' _v = b.run inp' pos' succ fail in
    a.run inp pos succ' fail
}

let (<* ) a b =
{
  run = fun inp pos succ fail ->
    let succ0 inp0 pos0 v =
      let succ1 inp1 pos1 _ = succ inp1 pos1 v in 
      b.run inp0 pos0 succ1 fail
    in
    a.run inp pos succ0 fail
}

let (<?>) p msg = 
{
  run = fun input pos success failure -> 
    let failure' inp' pos' msg' = failure inp' pos' (msg :: msg')
  in p.run input pos success failure'
}

let (<|>) p q = 
{
  run = fun input pos success failure ->
    let failure' inp' pos' _msg = q.run inp' pos' success failure
  in 
  p.run input pos success failure'
}

let advance n = 
{
  run = fun input pos success failure ->
    if pos + n > String.length input then failure input pos []
    else success input (pos + 1) ()
}

let take_while f = 
{
  run = fun input pos success _fail ->
    let rec collect acc pos = 
      if pos < String.length input && f (input.[pos]) then
        collect (acc ^ String.make 1 (input.[pos])) (pos + 1)
      else (acc, pos)
    in
    let (result, npos) = collect "" pos in
    success input npos result
}

let take_while1 f = 
{
  run = fun input pos success failure ->
    let rec collect acc pos = 
      if pos < String.length input && f (input.[pos]) then
        collect (acc ^ String.make 1 (input.[pos])) (pos + 1)
      else (acc, pos)
    in
    let (result, npos) = collect "" pos in
    if npos = pos then failure input pos []
    else success input npos result
}

let peek_char =
{
  run = fun input pos success _fail ->
    if String.length input <= pos then 
      success input pos None
    else
      success input pos (Some input.[pos])
}

let peek_string n =
{
  run = fun input pos success failure ->
    let rec collect i acc = 
      if i = n then success input pos acc
      else
        if pos + i >= String.length input then failure input pos []
        else collect (i + 1) (acc ^ (String.make 1 input.[pos+i] ))
    in collect 0 ""
}

let char c = 
{
  run = fun input pos success failure ->
    if pos > String.length input then failure input pos []
    else
      if pos >= String.length input then failure input pos []
      else
        if input.[pos] = c then success input (pos+1) c
        else failure input pos []
}

let string str =
{
  run = fun input pos success failure ->
    let n = String.length str in
    let rec collect i acc = 
      if i = n then success input pos acc
      else
        if pos + i >= String.length input then failure input pos []
        else 
          if str.[i] = input.[pos + i] then collect (i + 1) (acc ^ (String.make 1 input.[pos+i]))
          else failure input pos []
    in collect 0 ""
}

let cons x xs = x :: xs

let fix_lazy ~max_steps f =
  let steps = ref max_steps in
  let rec p = lazy (f r)
  and r = { run = fun inp pos fail succ ->
    decr steps;
    if !steps < 0
    then (
      steps := max_steps;
      State.Lazy (lazy ((Lazy.force p).run inp pos fail succ)))
    else
      (Lazy.force p).run inp pos fail succ
          }
  in
  r

let notset = { run = fun _buf _pos _fail _succ -> failwith "oopsy :/" }

let fix_direct f =
  let rec p = ref notset
  and r = { run = fun input pos fail succ ->
    (!p).run input pos fail succ }
  in
  p := f r;
  r

let fix =
  match Sys.backend_type with
  | Native -> fix_direct
  | Bytecode -> fix_direct
  | Other _ -> fun f -> fix_lazy ~max_steps:20 f

let sep_by1 s p =
  fix (fun m -> lift2 cons p ((s *> m) <|> return []))

let sep_by s p =
  (lift2 cons p ((s *> sep_by1 s p) <|> return [])) <|> return []

let pos = 
  { run = fun input pos succ _fail -> succ input pos pos }

let many p =
  fix (fun m ->
    (lift2 cons p m) <|> return [])
