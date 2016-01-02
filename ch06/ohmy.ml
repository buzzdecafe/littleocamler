type fruit = 
  Peach
  | Apple
  | Pear
  | Lemon
  | Fig
;;

type tree = 
  Bud
  | Flat of fruit * tree
  | Split of tree * tree
;;

(*
 * flat_only : tree -> bool
 *)
let rec flat_only = function
  Bud           -> true
  | Flat (f, t) -> flat_only t
  | _           -> false
;;

(*
 * split_only : tree -> bool
 *)
let rec split_only = function
  Bud           -> true
  | Split (t, t') -> split_only t && split_only t'
  | _           -> false
;;

let splittree = split_only 
  (Split (
    Split (Bud, (Split (Bud, Bud))),
    Split (Bud, (Split (Bud, Bud)))
    )
  )
;;

let rec contains_fruit = function
  Bud             -> false
  | Flat (f, t)   -> true
  | Split (t, t') -> contains_fruit t || contains_fruit t'
;;

let contains_fruit' t = not (split_only t);;

let larger_of (x, y) : int = if x > y then x else y;;

let rec height = function
  Bud -> 0
  | Flat (f, t)   -> 1 + (height t)
  | Split (t, t') -> 1 + (larger_of ((height t), (height t')))
;;

let h1 = height (Split (Bud, Bud));;

let eq_fruit (a: fruit) (b: fruit) = (a = b);;

let rec subst_in_tree newf oldf tree = 
  match tree with
  | Bud -> Bud
  | Flat (f, t) -> 
      if eq_fruit oldf f then (Flat (newf, (subst_in_tree newf oldf t)))
      else (Flat (f, (subst_in_tree newf oldf t)))
  | Split (t, t') -> Split ((subst_in_tree newf oldf t), (subst_in_tree newf oldf t'))
;;

let rec occurs fruit tree = 
  match tree with
  | Bud                        -> 0
  | Flat (f, t) when f = fruit -> 1 + (occurs fruit t)
  | Flat (_, t)                -> occurs fruit t
  | Split (t, t')              -> (occurs fruit t) + (occurs fruit t')
;;

(*
 * mutual recursion
 *)
type 'a slist = 
  Empty
  | Scons of (('a sexp) * ('a slist))
and
  'a sexp = 
    An_atom of 'a
  | A_slist of ('a slist)
;;

let rec 
occurs_in_slist fruit ls = 
  match ls with
  Empty          -> 0
  | Scons (s, y) -> (occurs_in_sexp fruit s) + (occurs_in_slist fruit y)
and
occurs_in_sexp fruit ls = 
  match ls with
  | An_atom f -> if eq_fruit f fruit then 1 else 0
  | A_slist y -> occurs_in_slist fruit y
;;

let rec 
subst_in_slist (newf: fruit) (oldf: fruit) (ls: fruit slist) : fruit slist = 
  match ls with
  | Empty          -> Empty
  | Scons (f, ls') -> 
      Scons ((subst_in_sexp newf oldf f), (subst_in_slist newf oldf ls'))
and
subst_in_sexp newf oldf s = 
  match s with
  | An_atom f -> if eq_fruit f oldf then An_atom newf else An_atom f
  | A_slist y -> A_slist (subst_in_slist newf oldf y)
;;

let eq_fruit_in_atom a = function
  | An_atom f  -> eq_fruit a f
  | A_slist ls -> false
;;

let rec
rem_from_slist a = function
  | Empty          -> Empty
  | Scons ((An_atom hd), tl) -> 
      if eq_fruit a hd 
      then rem_from_slist a tl
      else Scons ((An_atom hd), (rem_from_slist a tl))
  | Scons ((A_slist ls), tl) -> 
      Scons ((A_slist (rem_from_slist a ls)), (rem_from_slist a tl))
and
rem_from_sexp a = function
  | An_atom f -> An_atom f
  | A_slist y -> A_slist (rem_from_slist a y)
;;



