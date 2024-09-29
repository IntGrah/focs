type ('k, 'v) bst = Br of 'k * 'v * ('k, 'v) bst * ('k, 'v) bst | Lf

let rec insert k v = function
  | Lf -> Br (k, v, Lf, Lf)
  | Br (k', v', l, r) ->
    if k < k' then
      Br (k', v', insert k v l, r)
    else if k > k' then
      Br (k', v', l, insert k v r)
    else
      Br (k, v, l, r)

let rec union t = function
  | Lf -> t
  | Br (k, v, l, r) -> insert k v t |> union l |> union r

let rec slice x y = function
  | Lf -> Lf
  | Br (k, v, l, r) ->
    if x <= k && k <= y then
      Br (k, v, slice x y l, slice x y r)
    else if k < x then
      slice x y r
    else
      slice x y l

let rec delete k = function
  | Lf -> Lf
  | Br (k', v, l, r) ->
    if k = k' then begin
      match l, r with
      | Lf, r -> r
      | l, Lf -> l
      | l, r ->

    end
    else
