(* Author  : Pieter van Wyk
 * Created : 2020-04-21
 * Updated : 2020-04-21
 *
 * Solutions to the extra practice problems of week 4 of part A 
 *)

(* Problem 1.
* Write a function compose_opt : ('b -> 'c option) -> ('a -> 'b option) -> 'a -> 'c option
* that composes two functions with "optional" values. If either function returns NONE, then the result is NONE.
*)

fun compose_opt f_opt g_opt =
    fn x => (case (g_opt x) 
	     of NONE => NONE 
	      | SOME x' => f_opt x')
			                

(* Problem 2.
* Write a function do_until : ('a -> 'a) -> ('a -> bool) -> 'a -> 'a. do_until f p x will apply f to x
* and f again to that result and so on until p x is false. Example: do_until (fn x => x div 2) (fn x => x mod 2 <> 1)
* will evaluate to a function of type int->int that divides its argument by 2 until it reaches an odd number. 
* In effect, it will remove all factors of 2 its argument.
*)

(*
fun do_until f p =
    fn x => 
	*)
