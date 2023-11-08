type bool =
  | Var of string
  | Not of bool
  | And of bool * bool
  | Or  of bool * bool


(* Gets the union of two lists without duplicates *)
let union = List.fold_left (fun acc x -> if List.mem x acc then acc else x :: acc)


(* Finds all variable names in a boolean expression *)
let rec names = function
  | Var name -> [name]
  | Not expr -> names expr
  | And (expr1, expr2) -> union (names expr1) (names expr2)
  | Or  (expr1, expr2) -> union (names expr1) (names expr2)


(* Evaluates a boolean expression *)
let rec eval ctx = function
  | Var name -> List.mem name ctx
  | Not expr -> not (eval ctx expr)
  | And (expr1, expr2) -> eval ctx expr1 && eval ctx expr2
  | Or  (expr1, expr2) -> eval ctx expr1 || eval ctx expr2


(* Generates all possible contexts *)
let rec powerset = function
  | []     -> [[]]
  | h :: t -> let ps = powerset t in
              ps @ List.map (List.cons h) ps


(* Checks if the expression is true for all contexts *)
let is_tautology expr = List.for_all (fun ctx -> eval ctx expr) (powerset (names expr))


(* Tests *)

let expr1 = And (Var "a", (Or (Not (Var "b"), Var "c")))                   (* a && (!b || c) *)
let expr2 = Or (Var "a", Not (Var "a"))                                    (* a || !a *)
let expr3 = Or (Or (Var "a", Var "b"), And (Not (Var "a"), Not (Var "b"))) (* (a || b) || (!a && !b) *)

let () = [expr1; expr2; expr3]
  |> List.map is_tautology
  |> List.map string_of_bool
  |> List.iter print_endline
