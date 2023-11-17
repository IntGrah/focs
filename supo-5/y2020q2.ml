type colour = Red | Green | Blue


exception SizeMismatch

let rec feedback a b =
  match a, b with
  | []    , []       -> 0
  | []    , _
  | _     , []       -> raise SizeMismatch
  | h :: t, h' :: t' -> Bool.to_int (h = h') + feedback t t'


let test : colour list -> int = feedback [Blue; Green; Red]


let rec generate_lists = function
  | 0 -> [[]]
  | n -> List.concat_map (fun l -> [Red :: l; Green :: l; Blue :: l]) @@ generate_lists (n - 1)


let valid_lists b x = List.filter (fun a -> feedback a b = x) @@ generate_lists (List.length b)
