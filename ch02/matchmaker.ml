#use "../ch01/building.ml";;

type shish_kebab = 
  Skewer
  | Onion of shish_kebab
  | Lamb of shish_kebab
  | Tomato of shish_kebab
;;

let rec only_onions = function
  | Skewer   -> true
  | Onion x  -> only_onions x
  | Lamb _   -> false
  | Tomato _ -> false
;;

let oo = only_onions (Onion (Onion Skewer));;

let no = only_onions (Onion (Lamb Skewer));;

let rec is_vegetarian = function
  | Skewer   -> true
  | Onion x  -> is_vegetarian x
  | Lamb _   -> false
  | Tomato x -> is_vegetarian x
;;

let iv = is_vegetarian (Tomato (Onion Skewer));;
let nv = is_vegetarian (Tomato (Onion (Lamb Skewer)));;

type 'a shish = 
  Bottom of 'a
  | Onion of 'a shish
  | Lamb of 'a shish
  | Tomato of 'a shish
;;

(* possible `Bottom` type #1 *)
type rod = 
  Dagger
  | Fork
  | Sword
;;

(* possible `Bottom` type #2 *)
type plate = 
  Gold_plate
  | Silver_plate
  | Brass_plate
;;

(* matches aribtrary `Bottom` types, not just Skewer *)
let rec is_veggie = function
  | Bottom _ -> true
  | Onion x  -> is_veggie x
  | Lamb _   -> false
  | Tomato x -> is_veggie x
;;

let num_veggie = is_veggie (Onion (Tomato (Bottom (One_more_than Zero))));;

let rec what_bottom = function
  Bottom x   -> x
  | Onion x  -> what_bottom x
  | Lamb x   -> what_bottom x
  | Tomato x -> what_bottom x
;;

let is_52 = what_bottom (Bottom 52);;
let is_sword = what_bottom (Bottom Sword);;
let is_52' = what_bottom (Tomato (Onion (Lamb (Bottom 52))));;
