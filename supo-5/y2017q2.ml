type 'a stream = Cons of 'a * (unit -> 'a stream)


let rec map f (Cons (h, t)) = Cons (f h, fun () -> map f (t ()))


let mmul n = map (( * ) n)


let rec iterates f x = Cons (x, fun () -> iterates f (f x))


(* The lists are infinite, so the first list will never run out of elements *)
let rec interleave (Cons (h, t)) l =
  Cons (h, fun () -> interleave l (t ()))


let rec interleave3 (Cons (h, t)) l l' =
  Cons (h, fun () -> interleave3 l l' (t ()))


let rec pows579_unordered =
  Cons (1, fun () -> interleave3 (mmul 5 pows579) (mmul 7 pows579) (mmul 9 pows579))


let rec merge l l' =
  match l, l' with
  | Cons (h, t), Cons (h', t') ->
      if h < h' then
        Cons (h , fun () -> merge (t ()) l'     )
      else if h > h' then
        Cons (h', fun () -> merge l      (t' ()))
      else
        Cons (h , fun () -> merge (t ()) (t' ()))

let merge3 l l' l'' = merge (merge l l') l''


let rec pows579_ordered =
  Cons (1, fun () -> merge3 (mmul 5 pows579) (mmul 7 pows579) (mmul 9 pows579))


(* Merge sort on infinite lists is impossible because
 * the list cannot be broken into singleton sublists. *)


(* Tests *)

let rec take n (Cons (x, f)) =
    if n = 0 then [] else x :: take (n - 1) (f ())

let () = List.iter (Printf.printf "%d ") @@ take 20 pows579_ordered
