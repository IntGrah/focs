(* Tail must be invoked as t (), hence it is lazy *)
type 'a lazy_list =
  | Nil
  | Cons of 'a * (unit -> 'a lazy_list)


(* If s is empty return s', otherwise, pop the head of s and swap s and s' *)
let rec interleave s s' =
  match s with
  | Nil         -> s'
  | Cons (h, t) -> Cons (h, fun () -> interleave s' (t ()))


(* Apply f to the head and recurse *)
let rec map f = function
  | Nil         -> Nil
  | Cons (h, t) -> Cons (f h, fun () -> map f (t ()))


(* Cons x with iterates f (f x)*)
let rec iterates f x = Cons (x, fun () -> iterates f (f x))


(* This implementation includes duplicates. *)
let rec iterates2 f g x y =
  Cons ((x, y), fun () -> interleave (iterates2 f g x (g y)) (iterates2 f g (f x) y))


(* This implementation does not duplicate, but traverses in a weird order. *)
let rec iterates2 f g x y =
  Cons ((x, y), fun () -> interleave (map (fun y -> x, y) @@ iterates g (g y)) (iterates2 f g (f x) y))


(* This implementation traverses pairs of natural numbers in diagonal fashion, preventing duplication.
 * Given a diagonal of pairs, next_diag generates the next diagonal.
 *
 * @ = head
 * # = tail
 *
 * ....@-> fx
 * ...#|
 * ..#|v
 * .#|v
 * #|v
 * |v
 * v gy
 *
 * Both fx and gy are applied to the head. *)
let iterates2 f g x y =
  let fx (x, y) = (f x,   y) in (* Applies f to the first element of a pair *)
  let gy (x, y) = (  x, g y) in (* Applies g to the second element of a pair *)

  (* Concatenates a lazy list of non-lazy lists *)
  let rec concat = function
    | Nil               -> Nil
    | Cons ([]    , t') -> concat (t' ())
    | Cons (h :: t, t') -> Cons (h, fun () -> concat (Cons (t, t')))
  in
  
  let next_diag = function
    | []     -> [(x, y)]
    | h :: t -> fx h :: List.map gy (h :: t)
  in concat (iterates next_diag []) (* Concatenate all diagonals together. *)


let rec take n = function
  | Nil -> []
  | Cons (h, t) -> if n = 0 then [] else h :: take (n - 1) (t ())
