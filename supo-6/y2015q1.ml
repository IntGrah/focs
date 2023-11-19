(* Functional arrays are represented with binary trees.
 * To find element i, where i >= 1, obtain the binary
 * representation, drop the MSB, and read from
 * right to left, with 0 and 1 representing taking the
 * left or right branch respectively.
 * The total number of layers in the tree is log n, where
 * n is the number of elements. This is because the tree is
 * always balanced.
 * The lookup and update time is hence log n. *)


type 'a tree =
  | Lf
  | Br of 'a * 'a tree * 'a tree


let rec tcons v = function
  | Lf             -> Br (v, Lf, Lf)
  | Br (v', t, t') -> Br (v, tcons v' t', t)

let arrayoflist l = List.fold_right tcons l Lf


let rec merge l l' =
  match l, l' with
  | []    , l''
  | l''   , [] -> l''
  | h :: t, h' :: t' ->
      if h < h' then
        h  :: merge t l'
      else
        h' :: merge l t'


let rec filter_indices p = function
  | Lf -> []
  | Br (v, t, t') ->
    let j  = List.map (fun i -> 2 * i    ) (filter_indices p t ) in
    let j' = List.map (fun i -> 2 * i + 1) (filter_indices p t') in
    if p v then 1 :: merge j j' else merge j j'
