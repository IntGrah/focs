type 'a tree =
  | Twig of 'a
  | Br   of 'a * 'a tree * 'a tree


exception Backtrack

let find_path predicate root =
  let rec find_path_aux sum path = function
    | Twig v ->
        let path' = v :: path in
        let sum'  = v +  sum  in
        if predicate sum' then path'
        else raise Backtrack
    | Br (v, l, r) ->
        let path' = v :: path in
        let sum'  = v +  sum  in
        try
          find_path_aux sum' path' l
        with Backtrack ->
          find_path_aux sum' path' r
  in
  try List.rev (find_path_aux 0 [] root)
  with Backtrack -> raise Not_found
  (* If the root node attempts to backtrack it means no match was found. *)


let all_paths predicate root =
  let rec paths_aux sum path acc = function
    | Twig v ->
        let path' = v :: path in
        let sum'  = v +  sum  in
        if predicate sum' then path' :: acc else acc
    | Br(v, l, r) ->
        let path' = v :: path in
        let sum'  = v +  sum  in
        paths_aux sum' path' (paths_aux sum' path' acc r) l
  in
  List.map List.rev (paths_aux 0 [] [] root)


(* Tests *)

let example_tree = Br (1, Br (4, Twig 7, Twig 9), Br (2, Twig 4, Twig 3))

let prime n =
  let rec checkZero x = function
    | 1 -> true    
    | d -> (x mod d <> 0) && checkZero x (d-1)
  in match n with
  | 0 | 1 -> false
  | _ -> checkZero n (n-1)

let even n = n mod 2 = 0

let () = example_tree
  |> find_path prime
  |> List.map string_of_int
  |> String.concat "; "
  |> fun s -> "[" ^ s ^ "]"
  |> print_endline

let () = example_tree
  |> all_paths even
  |> List.map (List.map string_of_int)
  |> List.map (String.concat "; ")
  |> List.map (fun s -> "[" ^ s ^ "]")
  |> String.concat " "
  |> print_endline
