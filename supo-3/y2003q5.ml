module Stream = struct
  (* Nil represents an empty stream.
   * Cons (v, t) represents the head of a stream.
   * To access the tail, t must be invoked as t () because it is lazy.
   *)
  type 'a t =
    | Nil
    | Cons of 'a * (unit -> 'a t)


  (* Replaces the Nil of the first stream with the second stream.
   * The appending is also lazy, so it works with infinite streams.
   *)
  let rec append s s' =
    match s with
    | Nil         -> s'
    | Cons (v, t) -> Cons(v, fun () -> append (t ()) s')


  (* Take one item of the first stream.
   * Then swap the streams and repeat.
   *)
  let rec interleave s s' =
    match s with
    | Nil         -> s'
    | Cons (v, t) -> Cons (v, fun () -> interleave s' (t ()))


  (* Take n items of a stream.
   * Returns a stream.
   *)
  let rec take n = function
    | Nil         -> Nil
    | Cons (v, t) ->
        if n = 0 then Nil
        else Cons (v, fun () -> take (n - 1) (t ()))


  (* Convert stream to list.
   * The stream must be finite.
   *)
  let rec to_list = function
    | Nil         -> []
    | Cons (v, t) -> v :: to_list (t ())
end


let rec next_binary_word = function
  | []     -> [0]
  | 0 :: t -> 1 :: t
  | 1 :: t -> 0 :: next_binary_word t
  | _      -> failwith "Must be a list of 0s and 1s"


let binary_words =
  let rec words_from d =
    Stream.Cons (List.rev d, fun () -> words_from (next_binary_word d))
  in
  words_from []


let binary_palindromes =
  let rec palindromes_from d =
    let d' = List.rev d in
    let even  = List.rev_append d' d' in
    let odd_0 = List.rev_append d' (0 :: d') in
    let odd_1 = List.rev_append d' (1 :: d') in

    Stream.Cons (even, fun () ->
      Stream.Cons (odd_0, fun () ->
        Stream.Cons (odd_1, fun () ->
          palindromes_from (next_binary_word d)
        )
      )
    )
  in
  palindromes_from []


(* Tests *)

let naturals =
  let rec integers_from n =
    Stream.Cons (n, fun () -> integers_from (n + 1))
  in
  integers_from 0

let () = Stream.append naturals naturals
  |> Stream.take 10
  |> Stream.to_list
  |> List.map string_of_int
  |> String.concat " "
  |> print_endline

let () = Stream.interleave naturals naturals
  |> Stream.take 10
  |> Stream.to_list
  |> List.map string_of_int
  |> String.concat " "
  |> print_endline

let () = binary_words
  |> Stream.take 10
  |> Stream.to_list
  |> List.map (List.map string_of_int)
  |> List.map (String.concat "; ")
  |> List.map (fun str -> "[" ^ str ^ "]")
  |> String.concat " "
  |> print_endline

let () = binary_palindromes
  |> Stream.take 10
  |> Stream.to_list
  |> List.map (List.map string_of_int)
  |> List.map (String.concat "; ")
  |> List.map (fun str -> "[" ^ str ^ "]")
  |> String.concat " "
  |> print_endline
