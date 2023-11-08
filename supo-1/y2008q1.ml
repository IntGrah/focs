let rec prefix prefix list =
  match prefix, list with
  (* Empty list is a prefix of any list *)
  | [], _ -> true
  (* Non-empty list cannot be a prefix of empty list *)
  | _, [] -> false
  (* Make sure the heads match, then continue *)
  | x :: xs, y :: ys -> x == y && is_prefix xs ys

let rec sublist sub super =
  match sub, super with
  | [], _ -> true
  | _, [] -> false
  (* Attempt to match prefix, then drop the head of super and continue *)
  | _, h :: t -> prefix sub super || sublist sub t
