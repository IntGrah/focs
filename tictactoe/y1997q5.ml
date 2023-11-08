type player = PlayerO | PlayerX
type result = OWins   | XWins   | Draw
type cell   = O       | X       | Empty
type board = cell array
type state =
  | Finished of result
  | Ongoing 
type tree =
  | Leaf   of board * result
  | Branch of board * player * tree list


let state board : state =
  let line_indices = (* All possible indices of winning lines *)
    [[0; 1; 2]; [3; 4; 5]; [6; 7; 8]; (* Rows *)
     [0; 3; 6]; [1; 4; 7]; [2; 5; 8]; (* Columns *)
     [0; 4; 8]; [2; 4; 6]]            (* Diagonals *)
  in

  let check_line : cell list -> result = function
    | [O; O; O] -> OWins
    | [X; X; X] -> XWins
    | _         -> Draw
  in

  let winning_lines : result list = line_indices
    |> List.map (List.map (Array.get board)) (* Retrieve the cells along the line *)
    |> List.map check_line (* Check if there are 3 in a row *)
    |> List.filter ((<>) Draw) (* 3 in a row of O or X only *)
  in

  try Finished (List.hd winning_lines)
  with Failure _ -> (* If there are no winning lines *)
    if Array.for_all ((<>) Empty) board then (* If board is full *)
      Finished Draw (* The game is drawn *)
    else
      Ongoing (* The game is ongoing *)


let rec tree board player : tree =
  match state board with
    (* If game is finished, return a Leaf node of the board state and result *)
  | Finished res -> Leaf (board, res)
  | Ongoing ->
      let cell = if player = PlayerO then O else X in
      let other_player = if player = PlayerO then PlayerX else PlayerO in
      let indices = [0; 1; 2; 3; 4; 5; 6; 7; 8] in
      let children = indices
        (* You can only play in empty cells *)
        |> List.filter (fun i -> board.(i) = Empty)
        (* Copy the board, edit the cell, and return the board *)
        |> List.map (fun i -> let b = Array.copy board in b.(i) <- cell; b)
        (* Recursively compute game nodes with the other player to move *)
        |> List.map (fun board -> tree board other_player)
      in Branch (board, player, children)


let mktree () = tree (Array.make 9 Empty) PlayerO


let winner_is_O : tree -> int =
  let rec winner_aux acc = function
    | Leaf   (_, result)      -> acc + if result = OWins then 1 else 0
    | Branch (_, _, children) -> List.fold_left winner_aux acc children
  in winner_aux 0


let () = print_int @@ winner_is_O @@ mktree ()
