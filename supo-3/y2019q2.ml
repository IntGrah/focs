type nested_list =
  | Atom of int
  | Nest of nested_list list


let example = Nest [Nest [Atom 3; Atom 4]; Atom 5; Nest [Atom 6; Nest [Atom 7]; Atom 8]; Nest []]


let rec flatten = function
  | Atom n -> [n]
  | Nest [] -> []
  | Nest (h :: t) -> flatten h @ flatten (Nest t)


let flatten nested_list =
  let rec flatten_aux acc = function
    | Atom n        -> n :: acc
    | Nest []       -> acc
    | Nest (h :: t) -> flatten_aux (flatten_aux acc h) (Nest t)
  in List.rev (flatten_aux [] nested_list)


let rec nested_map (f : int -> int) = function
  | Atom n -> Atom (f n)
  | Nest l -> Nest (List.map (nested_map f) l)


let pack_as xs nested =
  let rec pack_aux acc = function
    | []     , Atom _        -> raise (Failure "Not enough items")
    | x :: xs, Atom _        -> (xs, Atom x)
    | xs     , Nest []       -> (xs, Nest (List.rev acc))
    | xs     , Nest (h :: t) ->
      let (xs', h') = pack_aux [] (xs, h) in
      pack_aux (h' :: acc) (xs', Nest t)
  in snd (pack_aux [] (xs, nested))


type nested_zlist =
  | ZAtom of int
  | ZNest of (unit -> nested_zlist) list


let rec convert = function
  | ZAtom n -> Atom n
  | ZNest l -> Nest (List.map (fun z -> convert (z ())) l)


(* Tests *)

let () = example
  |> pack_as [3; 1; 4; 1; 5; 9; 2; 6]
  |> flatten
  |> List.map string_of_int
  |> String.concat " "
  |> print_endline
