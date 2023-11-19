exception Empty


let rec poplast = function
  | []     -> raise Empty
  | [x]    -> []
  | h :: t -> h :: poplast t


let rec last = function
  | []     -> raise Empty
  | [x]    -> x
  | h :: t -> last t


let rec winning_diff coins =
  match coins with
  | []     -> 0
  | h :: t -> max (h - winning_diff t) (last coins - winning_diff (poplast coins))


module Deque = struct
  type 'a t = 'a list * 'a list
  
  let divide_pop l =
    let rec divide n l =
      if n = 0 then [], l else match l with
        | []     -> raise Empty
        | h :: t ->
            let taken, remaining = divide (n - 1) t in
            h :: taken, remaining
    in
    let front, back = divide (List.length l / 2) l in
    front, List.tl (List.rev back)

  let front = function
    | h :: _, _  -> h
    | []    , q' -> last q'

  let popfront = function
    | []    , [] -> raise Empty
    | []    , q' ->
        let front, back = divide_pop q' in
        back, front
    | h :: t, q' -> t, q'

  let last = function
    | q, h :: _ -> h
    | q, []     -> last q

  let poplast = function
    | [], []       -> raise Empty
    | q , []       -> divide_pop q
    | q , h' :: t' -> q, t'
end

(* T(n) = 2T(n - 1) + O(1) *)

(* O(2^n) because winning_diff calls itself twice, reduces the problem size by 1. *)
