(* Lazy lists are implemented as a pair of head and
 * a function returning the tail:
 * fun () -> tail
 * Greedy evaluation does not occur, because 'tail' is
 * not evaluated, which makes infinite lists possible. *)

module Stream = struct
  type 'a t = Cons of 'a * (unit -> 'a t)


  let tail (Cons (h, t)) = t ()


  let positive_ints =
    let rec ints_from n = Cons (n, fun () -> ints_from (n + 1)) in
    ints_from 1


  let rec map f (Cons (h, t)) = Cons (f h, fun () -> map f @@ t ())


  let rec diag (Cons (Cons (h, _), t')) = Cons (h, fun () -> diag @@ map tail @@ t' ())


  let rec outer_product f x y = map (fun x_i -> map (f x_i) y) x


  let rec interleave (Cons (h, t)) l' = Cons (h, fun () -> interleave l' @@ t ())


  let rec traverse (Cons (Cons (h, t), t')) = Cons (h, fun () -> interleave (t ()) @@ traverse @@ t' ())
end