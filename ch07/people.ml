let identity x = x;;
let true_maker x = true;;

type bool_or_int = 
  Hot of bool
  | Cold of int
;;

let hot_maker x = Hot x;;

let help f = 
  Hot (true_maker
    (if true_maker true then f else true_maker))
;;
(* else clause is unreachable *)

type chain = Link of (int * (int -> chain));;

let rec ints n = Link (n + 1, ints);;

let rec skips n = Link (n + 2, skips);;

let eq_int (a: int) (b: int) = (a = b);;

let divides_evenly n c = eq_int (n mod c) 0;;

let rec is_mod_5_or_7 n =
  if divides_evenly n 5 then true else divides_evenly n 7
;;

let rec some_ints n = 
  let next = (n + 1) in
  if is_mod_5_or_7 next then Link (next, some_ints)
  else some_ints next
;;

let rec chain_item n (Link (i, f)) = 
  if eq_int n 1 then i
  else chain_item (n - 1) (f i)
;;

let first_si = chain_item 1 (some_ints 0);;
let sixth_si = chain_item 6 (some_ints 0);;
let thirtyseventh_si = chain_item 37 (some_ints 0);;

let is_prime n = 
  let rec has_no_divisors n c = 
    if eq_int c 1 then true
    else 
      if divides_evenly n c then false
      else has_no_divisors n (c - 1)
  in 
  has_no_divisors n (n - 1)
;;

let rec primes n = 
  let next = (n + 1) in
  if is_prime next then Link (next, primes)
  else primes next
;;

let p12 = chain_item 12 (primes 1);;

let rec fibs n m = Link ((n + m), (fibs m));;

let fiblink0 = Link (0, (fibs 1));;

let fibs_1 m = Link ((1 + m), (fibs m));;
(* fibs_1 = fibs 1 *)

let fibs_2 = fibs 2;;




