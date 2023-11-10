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


let state board : state =
  let check_line (i, j, k) : result =
    let a, b, c = board.(i), board.(j), board.(k) in
    if a = b && b = c then a else None
  in

  match List.filter_map check_line line_indices with
  | h :: _ -> Finished (Some h) (* There exists a winning line *)
  | []     -> (* No winning lines *)
      if Array.for_all ((<>) None) board then (* Board is full *)
        Finished None
      else Ongoing


let rec tree board player : tree =
  match state board with
  | Finished res -> Lf (board, res)
  | Ongoing ->
      let other_player = if player = O then X else O in
      let children = [0; 1; 2; 3; 4; 5; 6; 7; 8]
        |> List.filter (fun i -> board.(i) = None) (* You can only play in empty cells *)
        |> List.map (fun i -> let b = Array.copy board in b.(i) <- Some player; b) (* Copy, edit, and return *)
        |> List.map (fun board -> tree board other_player) (* Recursively compute game nodes *)
      in Br (board, player, children)


let mktree () = tree (Array.make 9 None) O


let winner_is_O : tree -> int =
  let rec winner_aux acc = function
    | Lf (_, result     ) -> acc + if result = Some O then 1 else 0
    | Br (_, _, children) -> List.fold_left winner_aux acc children
  in winner_aux 0


let () = print_int @@ winner_is_O @@ mktree ()
