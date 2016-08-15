#use "../ch01/building.ml";;

type 'a pizza = 
  Bottom
  | Topping of ('a * ('a pizza))
;;

type fish = 
  Anchovy
  | Lox
  | Tuna
;;

(*
 * ungrammatical definition of the idea:
 *
type fish pizza = 
  Bottom
  | Topping of (fish * (fish pizza))
;;
*)

let rec rem_anchovy_1 = function
  | Bottom               -> Bottom
  | Topping (Anchovy, x) -> rem_anchovy_1 x
  | Topping (Tuna, x)    -> Topping (Tuna, rem_anchovy_1 x)
  | Topping (Lox, x)     -> Topping (Lox, rem_anchovy_1 x)
;;

let rec rem_anchovy = function
  | Bottom               -> Bottom
  | Topping (Anchovy, x) -> rem_anchovy x
  | Topping (f, x)       -> Topping (f, rem_anchovy x)
;;

let rec rem_tuna = function
  | Bottom            -> Bottom
  | Topping (Tuna, x) -> rem_tuna x
  | Topping (f, x)    -> Topping (f, rem_tuna x)
;;

let rec rem_fish (f : fish) p = 
  match p with
  | Bottom                      -> Bottom
  | Topping (f', x) when f' = f -> rem_fish f x
  | Topping (f', x)             -> Topping (f', (rem_fish f x))
;;

let eq_fish (a: fish) (b: fish) = a = b;;

let ch05_t1 = rem_fish Anchovy (Topping (Anchovy, Bottom));; (* => Bottom *)

let ch05_t2 = rem_fish Tuna 
  (Topping (Anchovy, 
    Topping (Tuna, 
      Topping (Anchovy, Bottom))));; (* => (Topping (Anchovy, (Topping (Anchovy, Bottom)))) *)

let eq_int (a : int) (b : int) = a = b;;

(*
let rem_int (n : int) p = 
  match p with
  | Bottom                      -> Bottom
  | Topping (f', x) when f' = f -> rem_int f x
  | Topping (f', x)             -> Topping (f', (rem_int f x))
;;
*)

let rec rem_int x p = 
  match p with
  | Bottom          -> Bottom
  | Topping (n, p') -> 
      if eq_int n x then rem_int x p'
      else Topping (n, (rem_int x p'))
;;

let rint = rem_int 3
  (Topping (3, 
    Topping (2,
      Topping (3,
        Topping (2, Bottom)))))
;;

let rec subst_fish new_fish old_fish p = 
  match p with
  | Bottom                            -> Bottom
  | Topping (f, p') when f = old_fish -> Topping (new_fish, (subst_fish new_fish old_fish p'))
  | Topping (f, p')                   -> Topping (f, (subst_fish new_fish old_fish p'))
;;

let refished = subst_fish Lox Anchovy 
  (Topping (Anchovy,
    Topping (Tuna,
      Topping (Anchovy, Bottom))))
;;

let rec subst_int new_n old_n p = 
  match p with
  | Bottom                         -> Bottom
  | Topping (f, p') when f = old_n -> Topping (new_n, (subst_int new_n old_n p'))
  | Topping (f, p')                -> Topping (f, (subst_int new_n old_n p'))
;;

let rec eq_num (a: num) (b: num) =
  match (a, b) with
  | (Zero, Zero)                       -> true
  | (One_more_than a, One_more_than b) -> eq_num a b
  | _                                  -> false
;;

let z0 = eq_num Zero Zero;;
let z1 = eq_num Zero (One_more_than Zero);;
let z2 = eq_num Zero (One_more_than (One_more_than Zero));;


