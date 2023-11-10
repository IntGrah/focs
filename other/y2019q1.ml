module type Natural = sig
  type t
  val to_int : t -> int
  val of_int : int -> t
  val add : t -> t -> t
end


module Peano : Natural = struct
  type t =
    | Zero
    | Succ of t

  let rec to_int = function
    | Zero   -> 0
    | Succ n -> 1 + to_int n

  let rec of_int = function
    | 0 -> Zero
    | n -> Succ (of_int (n - 1))
  
  let rec add a = function
    | Zero    -> a
    | Succ b' -> Succ (add a b')
end


module Binary : Natural = struct
  let int b = if b then 1 else 0

  type t = bool list

  let rec succ = function
    | [] -> [true]
    | false :: t -> true  :: t
    | true  :: t -> false :: succ t

  let rec to_int = function
    | []     -> 0
    | h :: t -> int h + 2 * to_int t

  let rec of_int = function
    | 0 -> []
    | n -> succ (of_int (n - 1))

  let add =
    let majority a b c = a && b || b && c || c && a in
    let rec add_aux carry a b =
      match a, b with
      | []    , []       -> if carry then [true] else []
      | n     , []
      | []    , n        -> if carry then succ n else n
      | h :: t, h' :: t' ->
          (carry <> h <> h') :: add_aux (majority carry h h') t t'
    in
    add_aux false
end


module Church = struct
  type 'a t = ('a -> 'a) -> 'a -> 'a

  let to_int n = n ((+) 1) 0

  let rec of_int = function
    | 0 -> fun f x -> x
    | n -> fun f x -> f (of_int (n - 1) f x)

  let add a b = fun f x -> a f (b f x)
end

let two f = Church.of_int 2 f
let three f = Church.of_int 3 f

let i = two three
let () = print_int (Church.to_int i)

let ii = three two
let () = print_int (Church.to_int ii)

let (>>) f g x = g (f x)

let iii = two >> three
let () = print_int (Church.to_int iii)

let iv = three >> two
let () = print_int (Church.to_int iv)
