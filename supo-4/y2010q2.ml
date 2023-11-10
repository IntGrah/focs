(* Translated from standard ML to OCaml *)

let op f (x, y) = f x y


(* Starts with an accumulator, e.
 * Combines e and each element of l together.
 *)
let rec foldl f (e, l) =
  match l with
  | [] -> e
  | x :: xs -> foldl f (f (e, x), xs)


(* The intermediate function foldl (op ( * ))
 * takes an acculumator and multiplies it by every element in a list.
 * The outer function does this for multiple lists.
 * Overall, f is the product of all elements in a list of list of integers. *)
let f x = foldl (foldl (op ( * ))) (1, x)


(* Takes a predicate p and a list.
 * The intermediate function takes a pair of lists,
 * and depending on p z, conses z to the first or second list.
 * Overall, the function g separates zs into two lists. *)
let g p zs = foldl (fun ((x, y), z) -> if p z then (z :: x, y) else (x, z :: y)) (([], []), zs)


(* Selection sort: O(n^2)
 * Finding the minimum takes linear time.
 * We have to find the minimum of the unsorted elements n times.
 * Overall it is quadratic. *)
let rec selection_sort =
  (* x: the current minimum element
   * l: untested elements
   * l': elements already tested
   * returns: a pair of the minimum element and the rest of the list *)
  let rec min x l l' =
    match l with
    | []     -> x, l'
    | h :: t ->
        if x < h then min x t (h :: l')
        else min h t (x :: l')
  in function
  | []     -> []
  | h :: t ->
      let (m, rest) = min h t []
      in m :: selection_sort rest


let mult_table n =
  let rec range acc n = if n = 0 then acc else range (n :: acc) (n - 1) in
  let r = range [] n in
  List.map (fun i -> List.map (fun j -> i * j) r) r


let table3d f n =
  let rec range acc n = if n = 0 then acc else range (n :: acc) (n - 1) in
  let r = range [] n in
  List.map (fun i -> List.map (fun j -> List.map (fun k -> f i j k) r) r) r
