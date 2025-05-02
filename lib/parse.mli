module State : sig type 'a t = Done of int * 'a | Fail of int * string list | Lazy of 'a t Lazy.t end 
type 'a with_input = string -> int -> 'a
type ('a, 'r) success = ('a -> 'r State.t) with_input
type 'r failure = (string list -> 'r State.t) with_input
type 'a t = {
  run : 'r. (('a, 'r) success -> 'r failure -> 'r State.t) with_input;
}
val fail_k : 'a -> int -> string list -> 'b State.t
val succ_k : 'a -> int -> 'b -> 'b State.t
val parse : 'a t -> string -> 'a State.t
val return : 'a -> 'a t
val fail : string list -> 'a t
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
val lift : 'a t -> ('a -> 'b) -> 'b t
val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
val lift4 :
  ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t
val ( *> ) : 'a t -> 'b t -> 'b t
val ( <* ) : 'a t -> 'b t -> 'a t
val ( <|> ) : 'a t -> 'a t -> 'a t
val ( <?> ) : 'a t -> string -> 'a t
val advance : int -> unit t

val take_while : (char -> bool) -> string t
val take_while1 : (char -> bool) -> string t
val peek_char : char option t
val peek_string : int -> string t
val char : char -> char t
val string : string -> string t
val fix : ('a t -> 'a t) -> 'a t
val sep_by1: 'a t -> 'b t -> 'b list t
val sep_by: 'a t -> 'b t -> 'b list t
val pos: int t
val many: 'a t -> 'a list t
