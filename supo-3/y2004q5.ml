type tr = N of int * (unit -> tr) * (unit -> tr)


let rec ndeep =
  let rec ndeep_aux acc n (N (v, l, r)) =
    if n = 0 then v :: acc
    else
      let acc' = ndeep_aux acc (n - 1) (r ()) in
      ndeep_aux acc' (n - 1) (l ())
  in ndeep_aux []


(* Uses a queue to perform breadth-first search.
 * Raises an exception when the required element is found.
 *)
exception Found of int

let bfs predicate tree =
  let rotate = function
    | [], back -> List.rev back, []
    | q        -> q
  in
  let rec bfs_aux = function
    | []              ,  _   -> failwith "Error"
    | N (v, l, r) :: t, back ->
        if predicate v then raise (Found v)
        else bfs_aux @@ rotate (t, r () :: l () :: back)
  in
  try bfs_aux ([tree], [])
  with Found v -> v


(* Tests *)

let rec cmb n =
  let left  = fun () -> cmb (n + 25) in 
  let right = fun () -> cmb (n - 1) in
  N (n, left, right)

let () = cmb 0
  |> ndeep 4
  |> List.map string_of_int
  |> String.concat " "
  |> print_endline

let () = cmb 0
  |> bfs ((<) 100)
  |> print_int
