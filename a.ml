let increasing =
  let rec aux run input =
    match run, input with
    | []    , [] -> []
    | r     , [] -> [r]
    | []    , x :: xs -> aux [x] xs
    | [a]   , x :: xs when x = a + 1 -> aux [a; x] xs
    | [a; b], x :: xs when x = b + 1 -> aux [a; x] xs
    | r     , i -> r :: aux [] i
  in aux []

type 'a llist = Nil | Cons of 'a * (unit -> 'a llist)

let increasing' =
  let rec aux run input =
    match run, input with
    | []    , Nil -> Nil
    | r     , Nil -> Cons (r, fun () -> Nil)
    | []    , Cons (x, xs) -> aux [x] @@ xs ()
    | [a]   , Cons (x, xs) when x = a + 1 -> aux [a; x] @@ xs ()
    | [a; b], Cons (x, xs) when x = b + 1 -> aux [a; x] @@ xs ()
    | r     , i -> Cons (r, fun () -> aux [] i)
  in aux []

let res = increasing [1; 5; 6; 7; 9; 10; 12; 15; 16; 20; 22; 23; 24; 25; 30]
