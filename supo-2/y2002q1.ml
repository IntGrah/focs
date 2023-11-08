type 'a tree =
  | Lf
  | Br of 'a * 'a tree * 'a tree
exception E

let rec path = function
  | Lf -> raise E
  | Br (v, t1, t2) ->
    try
      if v = 7 then []
      else 1 :: path t1
    with E -> 2 :: path t2


(* The function raises E if it is a leaf node.
 * It then checks if the node is a 7.
 * If so, we have the base case of the empty list.
 * Otherwise it tries the left path and appends 1 to the list.
 * If everything fails on the left path, it backtracks and tries the right path.
 * If everything fails on the right path, it raises E.
 * This either causes the parent call to backtrack, or the whole function to fail.
 *
 * Exampl
 * The nodes are searched in the order: 3, 5, 2, 7. *)

let paths t =
  let rec paths_aux path acc = function
    | Lf -> acc
    | Br(v, t1, t2) ->
        let acc' = if v = 7 then path :: acc else acc in
        paths_aux (1 :: path) (paths_aux (2 :: path) acc' t2) t1
  in List.map List.rev (paths_aux [] [] t)
