type tree =
  | Oak
  | Birch
  | Maple
  | Other of string


let describe = function
  | Oak        -> "Oak"
  | Birch      -> "Birch"
  | Maple      -> "Maple"
  | Other name -> name


let identify = function
  | "Oak"   -> Oak
  | "Birch" -> Birch
  | "Maple" -> Maple
  | name    -> Other name


(* The compiler checks if all possible constructors for tree
 * are present in the pattern matching.
 * For string pattern matching, the compiler can only confirm
 * comprehensiveness with a default pattern like 'name'. *)


type stree =
  | Oak
  | Birch
  | Maple


(* Pros: results are readily available
 * Cons: exceptions must be catched *)
let identify_exn = function
  | "Oak"   -> Oak
  | "Birch" -> Birch
  | "Maple" -> Maple
  | name    -> failwith "Unknown species"


(* Pros: forces developer to handle exceptional cases, thus reducing bugs.
 * Cons: results have to be matched and unwrapped. *)
let identify_opt = function
  | "Oak"   -> Some Oak
  | "Birch" -> Some Birch
  | "Maple" -> Some Maple
  | name    -> None

type stree =
  | Oak
  | Birch
  | Maple


(* val spotter : unit -> stree *)
let spotter =
  let state = ref [] in
  let seq = [Oak; Birch; Oak; Maple; Maple] in
  let rec next () = match !state with
  | []     -> state := seq; next ()
  | h :: t -> state := t; h
  in next


(* val spotter : 'a list -> 'a list -> 'a * 'a list 
 * Takes a sequence and a state.
 * Returns the next element and the next state *)
let rec spotter seq = function
  | [] -> spotter seq seq
  | h :: t -> h, t

let seq = [Oak; Birch; Oak; Maple; Maple]
let next, state = spotter seq []
(* val next : stree = Oak
 * val state : stree list = [Birch; Oak; Maple; Maple] *)
