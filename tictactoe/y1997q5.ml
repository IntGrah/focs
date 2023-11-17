type player = O | X
type cell = player option (* None = empty cell *)
type result = player option (* None = draw *)
type board = cell array
type state = Ongoing | Finished of result
type tree =
  | Lf of board * result
  | Br of board * player * tree list


let line_indices = (* All possible indices of winning lines *)
  [(0, 1, 2); (3, 4, 5); (6, 7, 8); (* Rows *)
   (0, 3, 6); (1, 4, 7); (2, 5, 8); (* Columns *)
   (0, 4, 8); (2, 4, 6)]            (* Diagonals *)


let state b : state =
  let check_line (i, j, k) : result = if b.(i) = b.(j) && b.(j) = b.(k) then b.(i) else None in
  let res = List.find_map check_line line_indices in
  if res <> None then (* There exists a winning line *)
    Finished res
  else if Array.mem None b then (* Board is not yet full*)
    Ongoing
  else (* Board is full *)
    Finished None


let rec tree p b : tree =
  match state b with
  | Finished res -> Lf (b, res)
  | Ongoing ->
      let opponent = if p = O then X else O in
      let children = [0; 1; 2; 3; 4; 5; 6; 7; 8]
        |> List.filter (fun i -> b.(i) = None) (* You can only play in empty cells *)
        |> List.map (fun i -> let b' = Array.copy b in b'.(i) <- Some p; b') (* Copy, edit, and return *)
        |> List.map (tree opponent) (* Recursively compute game nodes *)
      in Br (b, p, children)


let mktree () = tree O (Array.make 9 None)


let winner_is_O : tree -> int =
  let rec winner_aux acc = function
    | Lf (_, result     ) -> acc + if result = Some O then 1 else 0
    | Br (_, _, children) -> List.fold_left winner_aux acc children
  in winner_aux 0


let () = print_int @@ winner_is_O @@ mktree ()
