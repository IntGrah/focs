module type Collection = sig
  type 'a t
  val of_list : 'a list -> 'a t
  val get : 'a t -> int -> 'a
end


module Matrix (C : Collection) = struct
  type 'a t = 'a C.t C.t

  let create w m =
    let rec take r n m =
      if n = w && m = [] then
        []
      else if n > 0 then
        match m with
        | []     -> failwith "w does not divide length of m"
        | h :: t -> take (h :: r) (n - 1) t
      else
        C.of_list (List.rev r) :: (take [] w m)
    in C.of_list (take [] w m)

  let get r c m = C.get (C.get m r) c
end


module MatrixList = Matrix (struct
  include List
  let of_list = Fun.id
  let get = nth
end)


module MatrixArray = Matrix (Array)


module FuncArray = struct
  type 'a t =
    | Lf
    | Br of 'a * 'a t * 'a t

  let rec tcons v = function
    | Lf            -> Br (v, Lf, Lf)
    | Br (w, t, t') -> Br (v, tcons w t', t)

  let rec of_list l = List.fold_right tcons l Lf

  exception Subscript

  let rec sub n = function
    | Lf -> raise Subscript
    | Br (v, t, t') ->
        if n = 1 then v else
          sub (n / 2) (if n mod 2 = 0 then t else t')

  let get a n = sub (n + 1) a
end

module MatrixFuncArray = Matrix (FuncArray)


let m = MatrixList.create 3 [1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9.]
let mm = MatrixArray.create 3 [1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9.]
let mmm = MatrixFuncArray.create 3 [1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9.]
