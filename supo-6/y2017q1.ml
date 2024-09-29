(* The code checks if the state is winning.
 * If not, then it checks if any of the next states are winnable,
 * using recursion. This is because the || operator is
 * short circuiting, so if 'winning x' is true, it stops.
 *
 * This code fails when there is a cycle of states,
 * because it uses DFS and will fall into an infinite loop. *)


let next = function
 | 0 -> [1]
 | 1 -> [2;4]
 | 2 -> [3]
 | 3 -> [1]
 | _ -> []

let winning x = x = 4

let rec winnable x = winning x || List.exists winnable (next x)


(* Maintain a list of seen states (seen).
 * Iterate on a list of states by repeatedly:
 *   Checking if the list is empty, in which case it is not winnable.
 *   Checking if any of the states are winning.
 *   Applying 'next', and filtering out states already seen. *)

let winnable x =
  let rec winnable_aux seen states =
    let rec filter seen states' = function
      | []     -> winnable_aux seen states'
      | h :: t ->
          if List.mem h seen then
            filter seen states' t
          else
            filter (h :: seen) (h :: states') t
    in
    if states = [] then false else
      List.exists winning states ||
      filter seen [] (List.concat_map next states)
  in winnable_aux [x] [x]


(* The same, but also keep track of the paths. *)

let winpath x =
  let rec winpath_aux seen states =
    let rec filter seen states' = function
      | []     -> winpath_aux seen states'
      | h :: t ->
          let s = List.hd h in
          if List.mem s seen then
            filter seen states' t
          else
            filter (s :: seen) (h :: states') t
    in
    if states = [] then [] else try
      List.rev @@ List.find (fun p -> winning @@ List.hd p) states
    with Not_found ->
      let next_paths p = List.map (fun x -> x :: p) @@ next (List.hd p) in
      filter seen [] (List.concat_map next_paths states)
  in winpath_aux [x] [[x]]


(* Also restrict iterations to n. *)

let bounded_winpath x =
  let rec bounded_winpath_aux seen states n =
    if n < 0 then [] else
    let rec filter seen states' = function
      | []     -> bounded_winpath_aux seen states' (n - 1)
      | h :: t ->
          let s = List.hd h in
          if List.mem s seen then
            filter seen states' t
          else
            filter (s :: seen) (h :: states') t
    in
    if states = [] then [] else try
      List.rev @@ List.find (fun p -> winning @@ List.hd p) states
    with Not_found ->
      let next_paths p = List.map (fun x -> x :: p) @@ next (List.hd p) in
      filter seen [] (List.concat_map next_paths states)
  in bounded_winpath_aux [x] [[x]]

let p l = print_endline @@ String.concat " " @@ List.map string_of_int l

let () = p @@ bounded_winpath 0 5
let a = winnable 0
