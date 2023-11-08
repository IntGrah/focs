let rec insert x = function
  | []     -> [[x]] (* Only one way to insert into an empty list *)
  | h :: t -> (x :: l) :: List.map (List.cons h) (insert x t) (* Cons x to each element *)


(* Permuting n items is the same as permuting n-1 of them and inserting the other in every place *)
let rec permutations = function
  | [] -> [[]] (* 0! = 1*)
  | h :: t -> t
      |> permutations
      |> List.map (insert h)
      |> List.concat
