exception Too_small;;

let eq_int (a: int) (b: int) = (a = b);;

let is_zero_1 = eq_int 0;;

let pred_1 n = 
  if is_zero_1 n then raise Too_small
  else (n - 1);;

let succ_1 n = (n + 1);;

let rec plus_1 n m = 
  if is_zero_1 n then m
  else succ_1 (plus_1 (pred_1 n) m)
  (* why not plus (pred n) (succ m) ? *)
;;

type num = 
  Zero
  | One_more_than of num
;;

let is_zero = function
  | Zero -> true
  | _    -> false
;;

let pred = function
  | Zero            -> raise Too_small
  | One_more_than n -> n
;;

let succ n = One_more_than n;;

let rec plus n m = 
  if is_zero n then m
  else succ (plus (pred n) m)


let two = One_more_than (One_more_than Zero);;
let three = One_more_than (One_more_than (One_more_than Zero));;

let five = plus two three;;

(* plus & plus_1 have virtually the same definition; how can we make plus polymorphic? *)

(* things needed to make `plus`:
   * the type
   * Too_small exceptio
   * succ : number -> number
   * pred : number -> number
   * is_zero : number -> bool
*)
module type N = 
  sig
    type number
    exception Too_small
    val succ : number -> number
    val pred : number -> number
    val is_zero : number -> bool
  end
;;

(* Peano numbers *)
module NumberAsNum : N = 
  struct
    type num = Zero | One_more_than of num
    type number = num
    exception Too_small
    let succ n = One_more_than n
    let pred = function
      | Zero            -> raise Too_small
      | One_more_than n -> n
    let is_zero = function
      | Zero -> true
      | _    -> false
  end
;;

(* ints *)
module NumberAsInt : N = 
  struct
    type number = int
    exception Too_small
    let succ n = n + 1
    let pred n = if eq_int n 0 then raise Too_small else (n - 1)
    let is_zero = eq_int 0
  end
;;


module IntStruct = NumberAsInt;;

(* still driving toward unified `plus` *)

module type P = 
  sig
    type number
    val plus : number -> number -> number
  end
;;

module PON (ANum: N) : P = 
  struct
    type number = ANum.number
    let rec plus n m = 
      if ANum.is_zero n then m 
      else ANum.succ (plus (ANum.pred n) m)
  end
;;

module IntArith = PON (NumberAsInt);;
module NumArith = PON (NumberAsNum);;

(*
 * Doesn't work yet:
utop # IntArith.plus 1 2;;
Error: This expression has type int but an expression was expected of type IntArith.number
*)

(* "Numbers with Conceal and reveal" *)
module type N_C_R = 
  sig 
    type number
    exception Too_small
    val conceal : int -> number
    val succ : number -> number
    val pred : number -> number
    val is_zero : number -> bool
    val reveal : number -> int
  end
;;

(* OK, let's try this again *)
(* Peano numbers *)
module NumberAsNum : N_C_R = 
  struct
    type num = Zero | One_more_than of num
    type number = num
    exception Too_small
    let rec conceal =  function
      | 0 -> Zero
      | n -> One_more_than (conceal (n - 1))
    let succ n = One_more_than n
    let pred = function
      | Zero            -> raise Too_small
      | One_more_than n -> n
    let is_zero = function
      | Zero -> true
      | _    -> false
    let rec reveal = function
      | Zero            -> 0
      | One_more_than n -> 1 + (reveal n)
  end
;;

(* ints *)
module NumberAsInt : N_C_R = 
  struct
    type number = int
    exception Too_small
    let conceal n = n
    let succ n = n + 1
    let pred n = if eq_int n 0 then raise Too_small else (n - 1)
    let is_zero = eq_int 0
    let reveal n = n
  end
;;

module IntStruct = NumberAsInt;;
module NumStruct = NumberAsNum;;
module IntArith = PON (IntStruct);;
module NumArith = PON (NumStruct);;

let ex10_1 = NumStruct.reveal (NumStruct.succ (NumStruct.conceal 0));;

(* aaaaarrrrrgh still mis-matching types in this example:
let ex10_2 = NumStruct.reveal 
  (NumArith.plus (NumStruct.conceal 1) (NumStruct.conceal 2))
;;
*)

(* so let's inject the number type into PON so everyone is happy *)
module PON (ANum: N) : P with type number = ANum.number = 
  struct
    type number = ANum.number
    let rec plus n m = 
      if ANum.is_zero n then m 
      else ANum.succ (plus (ANum.pred n) m)
  end
;;

module IntStruct = NumberAsInt;;
module IntArith = PON (IntStruct);;

module NumStruct = NumberAsNum;;
module NumArith = PON (NumStruct);;

let ex10_2 = NumStruct.reveal 
  (NumArith.plus (NumStruct.conceal 1) (NumStruct.conceal 2))
;;
(* yay! now it works! That seems like a lot of effort to add 1 + 2! *)

let ex10_3 = IntStruct.reveal 
  (IntArith.plus (IntStruct.conceal 1) (IntStruct.conceal 2))
;;

(* There is another way out ... *)

(* ints *)
module NumberAsInt2 : N with type number = int = 
  struct
    type number = int
    exception Too_small
    let succ n = n + 1
    let pred n = if eq_int n 0 then raise Too_small else (n - 1)
    let is_zero = eq_int 0
  end
;;

module IntStruct2 = NumberAsInt2;;
module IntArith2 = PON (IntStruct2);;

let ex10_4 = IntArith2.plus 1 2;;

(* nums *)
module NumberAsNum2 : N with type number = num = 
  struct
    type number = num 
    exception Too_small
    let succ n = One_more_than n
    let pred = function
      | Zero            -> raise Too_small
      | One_more_than n -> n
    let is_zero = function
      | Zero -> true
      | _    -> false
  end
;;

module NumStruct2 = NumberAsNum2;;
module NumArith2 = PON (NumStruct2);;


let ex10_5 = NumArith2.plus (One_more_than Zero) (One_more_than (One_more_than Zero));;

module type S = 
  sig
    type number1
    type number2
    val similar : number1 -> number2 -> bool
  end
;;

module Same (ANum: N) (BNum: N) : (S with type number1 = ANum.number and type number2 = BNum.number) = 
  struct
    type number1 = ANum.number
    type number2 = BNum.number
    let rec sim n m = 
      if ANum.is_zero n then BNum.is_zero m
      else sim (ANum.pred n) (BNum.pred m)
    let similar n m = 
      try sim n m with
      | ANum.Too_small -> false
      | BNum.Too_small -> false
  end
;;

module SimIntNum = Same (IntStruct) (NumStruct);;
module SimNumInt = Same (NumStruct) (IntStruct);;

let ex10_6 = SimNumInt.similar (NumStruct.conceal 0) (IntStruct.conceal 0);;


(*
 * Still not there yet!!!
 *)
let new_num_plus x y = 
  NumStruct.reveal 
    (NumArith.plus 
      (NumStruct.conceal x)
      (NumStruct.conceal y))
;;

let new_int_plus x y = 
  IntStruct.reveal 
    (IntArith.plus 
      (IntStruct.conceal x)
      (IntStruct.conceal y))
;;

module type J = 
  sig
    val new_plus : int -> int -> int
  end
;;

module NP (ANum : N_C_R) (AP : P with type number = ANum.number) : J = 
  struct
    let new_plus x y = 
      ANum.reveal 
        (AP.plus 
          (ANum.conceal x)
          (ANum.conceal y))
  end
;;

module NPStruct = NP (NumberAsNum) (NumArith);;

(* wow, that's it? All that work to get here? *)

(* and now _times_ *)
module type T = 
  sig
    type number
    val times : number -> number -> number
  end
;;

module TON (ANum : N) (AP : P with type number = ANum.number) : T with type number = ANum.number = 
  struct
    type number = ANum.number
    let rec times n m =
      if ANum.is_zero m then m
      else AP.plus n (times n (ANum.pred m))
  end
;;

(* Is that all there is? *)



