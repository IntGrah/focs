module type Natural = sig
  type t
  val zero : t
  val succ : t -> t
  val add : t -> t -> t
  val to_int : t -> int
  val of_int : int -> t
end


module Peano : Natural = struct
  type t =
    | Zero
    | Succ of t

  let zero = Zero

  let succ n = Succ n
  
  let rec add a = function
    | Zero    -> a
    | Succ b' -> Succ (add a b')

  let rec to_int = function
    | Zero   -> 0
    | Succ n -> Int.succ (to_int n)

  let rec of_int = function
    | 0 -> Zero
    | n -> Succ (of_int (n - 1))
end


module Binary : Natural = struct
  type t = bool list

  let zero = []

  let rec succ = function
    | []         -> [true]
    | false :: t -> true  :: t
    | true  :: t -> false :: succ t

  let add =
    let majority a b c = a && b || b && c || c && a in
    let rec add_aux carry a b =
      match a, b with
      | []    , []       -> if carry then [true] else []
      | n     , []
      | []    , n        -> if carry then succ n else n
      | h :: t, h' :: t' ->
          (carry <> h <> h') :: add_aux (majority carry h h') t t'
    in add_aux false

  let rec to_int = function
    | []     -> 0
    | h :: t -> Bool.to_int h + 2 * to_int t

  let rec of_int = function
    | 0 -> []
    | n -> succ (of_int (n - 1))
end


module Church = struct
  type 'a t = ('a -> 'a) -> 'a -> 'a

  let zero f x = x

  let succ n f x = f (n f x)

  let add a b f x = a f (b f x)

  let to_int n = n Int.succ 0

  let rec of_int n f x =
    if n = 0 then x
    else f (of_int (n - 1) f x)
end


let two = fun f -> Church.of_int 2 f
let three = fun f -> Church.of_int 3 f

(* thrice, thrice is the same as 9 times *)
let () = print_int @@ Church.to_int (two three)

(* twice twice twice is the same as 8 times *)
let () = print_int @@ Church.to_int (three two)

let (>>) f g x = g (f x)

(* twice thrice is the same as 6 times *)
let () = print_int @@ Church.to_int (two >> three)

(* thrice twice is the same as 6 times *)
let () = print_int @@ Church.to_int (three >> two)
