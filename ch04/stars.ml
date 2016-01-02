type meza = 
  Shrimp
  | Calamari
  | Escargots
  | Hummus
;;

type main = 
  Steak
  | Ravioli
  | Chicken
  | Eggplant
;;

type salad = 
  Green
  | Cucumber
  | Greek
;;

type dessert = 
  Sundae
  | Mousse
  | Torte
;;

let meal_1 = (Calamari, Ravioli, Greek, Sundae);;
let meal_2 = (Hummus, Steak, Green, Torte);;

let mealb_1 = (Torte, Hummus, Steak, Sundae);;

let tinymeal_1 = (Shrimp, Sundae);;

let add_a_steak (x : meza) = (x, Steak);;

let eq_main = function
  | (Steak, Steak)       -> true
  | (Ravioli, Ravioli)   -> true
  | (Chicken, Chicken)   -> true
  | (Eggplant, Eggplant) -> true
  | _                    -> false
;;

let has_steak = function
  | ((_ : meza), Steak, (_ : dessert)) -> true
  | _              -> false
;;




