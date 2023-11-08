 (* O(1): A single operation is constant time. *)
let square x = x * x


(* O(n): Each element of the list is accessed once, so it is linear. *)
let rec sum = function
  | []     -> 0
  | h :: t -> h + sum t


(* O(n log n): The list is halved until singletons.
 * Then, for each iteration pairs of lists are merged.
 * This takes n time. However, there are log n iterations.
 *)
let rec merge l l' =
  match l, l' with
  | [],     _        -> l'
  | _,      []       -> l
  | h :: t, h' :: t' ->
      if h < h' then h :: merge t l'
      else h' :: merge l t'

let rec split = function
  | []              -> [],  []
  | [x]             -> [x], []
  | h :: h' :: rest ->
      let t, t' = split rest in
      h :: t, h' :: t'

let rec merge_sort = function
  | []  -> []
  | [x] -> [x]
  | l   ->
      let left, right = split l in
      merge (merge_sort left) (merge_sort right)


(* O(n^2): Each element gets multiplied with every element, so it is quadratic. *)
let mult_table l = l
  |> List.map (fun e -> List.map (( * ) e) l)
  |> List.concat


(* O(2^n). The function calls itself twice, so it is exponential. *)
let rec fibonacci = function
  | 0 -> 0
  | 1 -> 1
  | x -> fibonacci (x - 1) + fibonacci (x - 2)
