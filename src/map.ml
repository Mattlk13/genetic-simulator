(* ========== Definition ========== *)

(* A map is a 2D matrix of integers (food quantity) *)

type map =
  { size : int;
    cells : int array array;
  }

(* ========== Map creation ========== *)

(* Creates a map and randomly adds food on it *)
(* val create : size:int -> map *)
let create ~size : map =
  let f _ =
    let r = Random.int 100 in
    if (r < 50) then 40
    else if (r < 90) then 0
    else 10
  in
  let g _ = Array.init size f in
  { size = size;
    cells = Array.init size g }