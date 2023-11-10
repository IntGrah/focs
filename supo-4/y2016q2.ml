let sieve n =
  (* List of numbers from 2 to n inclusive *)
  let rec range acc n = if n < 2 then acc else range (n :: acc) (n - 1) in
  let rec sieve_aux = function
    | []     -> []
    (* Filter numbers which are not multiples of h *)
    | h :: t -> h :: sieve_aux (List.filter (fun x -> x mod h > 0) t)
  in sieve_aux (range [] n)


(* Too easy :P
 * "Well-known utility functions may be assumed to be available" *)
let remove_duplicates l = List.sort_uniq compare l


let rec merge l l' =
  match l, l' with
  | [],     _        -> l'
  | _,      []       -> l
  | h :: t, h' :: t' ->
      if h < h' then
        h  :: merge t l'
      else if h > h' then
        h' :: merge l t'
      else (* h = h' *)
        h  :: merge t t'

let rec split = function
  | []              -> [],  []
  | [x]             -> [x], []
  | h :: h' :: rest ->
      let t, t' = split rest in
      h :: t, h' :: t'

let rec merge_sort_uniq = function
  | []  -> []
  | [x] -> [x]
  | l   ->
      let left, right = split l in
      merge (merge_sort_uniq left) (merge_sort_uniq right)


let can_be_made chunks goal =
  let rec dfs acc =
    acc = goal ||
    let children = chunks
      |> List.map (String.cat acc)
      |> List.filter (fun acc' -> String.starts_with ~prefix:acc' goal)
    in List.exists dfs children
  in dfs ""

let () = "abracadabra"
  |> can_be_made ["ab"; "bra"; "abra"; "cad"]
  |> string_of_bool
  |> print_endline
