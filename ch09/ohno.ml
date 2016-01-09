type 'a list = Empty | Cons of 'a * 'a list;;

type box = Bacon | Ix of int;;

let is_bacon = function
  |Bacon -> true
  | _    -> false
;;

exception No_bacon of int;;

let rec where_is = function 
  | Empty              -> raise (No_bacon 0)
  | Cons (a_box, rest) -> 
      if is_bacon a_box then 1 
      else 1 + (where_is rest)
;;

let ex9_1 = try where_is 
  (Cons ((Ix 5),
    (Cons ((Ix 13), 
      (Cons (Bacon,
        (Cons ((Ix 8),
          Empty))))))))
  with No_bacon x -> x
;; (* -> 3  *)

let ex9_2 = try where_is 
  (Cons (Bacon, 
    (Cons ((Ix 8), Empty))))
  with No_bacon x -> x
;; (* -> 1  *)

let ex9_3 = try where_is 
  (Cons ((Ix 5),
    (Cons ((Ix 13),
      (Cons ((Ix 8), Empty))))))
  with No_bacon x -> x
;; (* -> 0 *)

exception Out_of_range;;

let eq_int (a: int) (b: int) = (a = b);;

let rec list_item n = function
  | Empty             -> raise Out_of_range
  | Cons (abox, rest) ->
      if eq_int n 1 then abox
      else list_item (n - 1) rest
;;


let rec 
find_1 n boxes = 
  check_1 n boxes (list_item n boxes)
and
check_1 n boxes = function
  | Bacon -> n
  | Ix n  -> find_1 n boxes
;;

let t = Cons ((Ix 5),
  (Cons ((Ix 4), 
    (Cons (Bacon,
      (Cons ((Ix 2), 
        (Cons ((Ix 7), 
          Empty)))))))))
;;

let oor_1 = try find_1 1 t with Out_of_range -> -1;; (* Out_of_range at (Ix 7) so -1 *)

let rec find n boxes = 
  try check n boxes (list_item n boxes)
  with Out_of_range -> find (n / 2) boxes
and check n boxes = function
  | Bacon -> n
  | Ix i  -> find i boxes
;;

let oor = find 1 t;; (* Bacon found at 3 *)

let rec path n boxes = 
  Cons (n, (
    try check boxes (list_item n boxes)
    with Out_of_range -> path (n / 2) boxes))
and check boxes = function (* shadowing prior binding of `check` *)
  | Bacon -> Empty
  | Ix i  -> path i boxes
;;

let path_test_1 = path 1 t;; (* path to the Bacon *)



