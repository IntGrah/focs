module type Grp = sig
  type t
  val add : t -> t -> t
  val neg : t -> t
  val zero : t
end

module type CRing = sig
  include Grp
  val mul : t -> t -> t
  val one : t
end

module Polynomial = struct
  module type P = sig
    include Grp
    type r
    module Context : Map.S with type key = string
    val eval : r Context.t -> t -> r
    val make : t -> t
    val to_string : (r -> string) -> t -> string
  end

  module type Over = functor (R : CRing) -> P
    with type r = R.t
    with type t = (R.t * (string * int) list) list

  module Over : Over = functor (R : CRing) -> struct
    type r = R.t
    type term = R.t * (string * int) list
    type t = term list
  
    let rec add p p' =
      match p, p' with 
      | [], q | q, [] -> q
      | (c, x) :: t, (c', x') :: t' ->
          match compare x x' with
          | -1 -> (c , x ) :: add t p'
          |  1 -> (c', x') :: add p t'
          |  _ ->
              let c'' = R.add c c' in
              let t'' =   add t t' in
              if c'' = R.zero then t''
              else (c'', x) :: t''
  
    let neg = List.map (fun (c, x) -> R.neg c, x)
  
    let zero = []
  
    let rec pow b = function
      | 0 -> R.one
      | 1 -> b
      | n ->
          let s = pow (R.mul b b) (n / 2) in
          if n mod 2 = 0 then s else R.mul b s
          
    module Context = Map.Make(String)

    let eval context p =
      let eval_term (c, x) = x
        |> List.map (fun (v, n) -> pow (Context.find v context) n)
        |> List.fold_left R.mul c
      in List.fold_left R.add R.zero @@ List.map eval_term p

    let make p =
      let rec insert p (c, x) =
        match p with
        | []                           -> [(c, x)]
        | (c', x') :: rest when x = x' -> (R.add c c', x) :: rest
        | term     :: rest             -> term :: insert rest (c, x)
      in
      let filter_zero_powers =
        List.filter (fun (_, n) -> n <> 0)
      in p
      |> List.map (fun (c, x) -> c, List.sort compare @@ filter_zero_powers x)
      |> List.fold_left insert []
      |> List.filter (fun (c, _) -> c <> R.zero)
      |> List.sort (fun a b -> compare (snd a) (snd b))

    let to_string str p =
      let term_to_string (c, x) = x
        |> List.map (fun (v, n) -> if n = 1 then v else v ^ "^" ^ string_of_int n)
        |> List.cons (if c = R.one then "" else str c)
        |> String.concat ""
      in String.concat "+" @@ List.map term_to_string p
  end
end


(* Tests *)

module IntPoly = Polynomial.Over (Int)

let p1 = IntPoly.make [3, [("x", 1)];  2, [("x", 2)]; 4, []; -4, []; 5, [("y", 1)]] (* 3x + 2x^2 + 4 - 4 + 5y *)
let p2 = IntPoly.make [1, [("x", 2)]; -3, [("x", 1)]; 2, [        ]]                (* x^2 - 3x + 2 *)
let p3 = IntPoly.make [2, [        ]; -3, [("x", 1)]; 1, [("x", 2)]]                (* 2 - 3x + x^2 *)

let res = IntPoly.add p1 p2

let () = List.iter print_endline @@ List.map (IntPoly.to_string string_of_int) [p1; p2; res; p3]

let () = print_endline @@ string_of_bool (p2 = p3)

let ctx = IntPoly.Context.(empty |> add "x" 4 |> add "y" 3)
 
let () = print_int @@ IntPoly.eval ctx res
