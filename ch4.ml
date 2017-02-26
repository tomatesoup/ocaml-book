(* Questions *)

(* 1. Write a function evens which does the opposite to  odds,returning the even numbered elements in a *)
(* list. For example, evens [2; 4; 2; 4; 2] should return [4; 4]. What is the type of your function? *)

let rec odd_elements l = match l with
    [] -> []
  | [a] -> [a]
  | a::_::t -> a :: odd_elements t
;;

(* evens: a list -> a list *)

let rec evens l =
  match l with
  | [] -> []
  | [b] -> []
  | _::b::t -> b :: evens t
;;

(* 2. Write a function count_true which counts the number of true elements in a list.
For example, count_true [true; false; true] should return 2. What is the type of your function?
Can you write a tail recursive version? *)

let rec count_true l =
  match l with
  | [] -> 0
  | true::t -> 1 + count_true t
  | false::t -> count_true t
;;

(* 3. Write a function which, given a list, builds a palindrome from it.
A palindrome is a list which equals its own reverse. You can assume the existence of rev and @.
Write another function which determines if a list is a palindrome. *)

let rec rev l = match l with
  [] -> []
  | h::t -> rev t @ [h]
;;

let rec make_palindrome l =
  match l with
  | [] -> []
  | h::t -> rev t @ [h]
;;

let rec is_palindrome a b =
  match a with
  | [] -> false
  | h::t -> a = rev b
;;

(* 4. Write a function drop_last which returns all but the last element of a list.
If the list is empty, it should return the empty list. So, for example,
drop_last [1; 2; 4; 8] should return [1; 2; 4]. What about a tail recursive version? *)

let rec drop_last l =
  match l with
  | [] -> []
  | [h] -> []
  | h::t -> h :: drop_last t
;;

(* 5. Write a function member of type α → α list → bool which returns true if an
 element exists in a list, or false if not. For example, member 2 [1; 2; 3]
 should evaluate to true, but member 3 [1; 2] should evaluate to false.*)

let rec member e l =
  match l with
  | [] -> false
  | h::t ->
    if h = e then true else member e t
;;

(* 6. Use your member function to write a function make_set which, given a list,
returns a list which contains all the elements of the original list, but has no
duplicate elements. For example, make_set [1; 2; 3; 3; 1] might return [2; 3; 1].
What is the type of your function? *)

let rec make_set_inner l u =
  match l with
  | [] -> u
  | h::t ->
    if (member h u) then make_set_inner t u else make_set_inner t (h :: u)
;;

let make_set l =
  make_set_inner l []
;;

(* 7. Can you explain why the rev function we defined is inefficient? How does
the time it takes to run relate to the size of its argument? Can you write a
more efficient version using an accumulating argument? What is its efficiency
in terms of time taken and space used? *)

let rec rev_inner l r =
  match l with
  | [] -> r
  | h::t -> rev_inner t (h :: r)
;;

let rev l =
  rev_inner l []
;;
