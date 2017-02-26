(* Questions *)
(* 1. Write a function which multiplies a given number by ten. What is its type? *)

let timesten x =
  x * 10;;

(* 2. Write a function which returns true if both of its arguments are non-zero, and false otherwise. What is the type of your function? *)

let arepositives a b =
  a > 0 && b > 0;;

(* 3. Write a recursive function which, given a number n, calculates the sum 1+2+3+...+n. What is its type? *)

let rec addup n =
  if n = 1 then 1 else n + addup (n - 1);;

(* 4. Write a function power x n which raises x to the power n. Give its type. *)

let rec power x n =
  match n with
  | 0 -> 1
  | 1 -> x
  | n -> x * power x (n - 1);;

(* 5. Write a function isconsonant which, given a lowercase character in the range 'a'...'z',determines if it is a consonant. *)

let isconsonant c =
  if c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u' then false else true;;

(* refactored *)

let isvowel c =
  c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u';;

let isconsonant c =
  not (isvowel c);;

(* 6. What is the result of the expression let x = 1 in let x = 2 in x + x? *)
Warning 26: unused variable x.
- : int = 4

(* 7. Can you suggest a way of preventing the nontermination of the factorial function in the case of a zero or negative argument? *)
let rec factorial a =
  if a <= 0 then 0 else if a = 1 then 1 else a * factorial (a - 1);;
