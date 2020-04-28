(* Author  : Pieter van Wyk
 * Created : 2020-04-21
 * Updated : 2020-04-28
 *
 * Solutions to the extra practice problems of week 4 of part A 
 *)

(* Problem 1.
* Write a function compose_opt : ('b -> 'c option) -> ('a -> 'b option) -> 'a -> 'c option
* that composes two functions with "optional" values. If either function returns NONE, then the result is NONE.
*)

fun compose_opt f_opt g_opt = fn x =>
    case (g_opt x) 
    of NONE => NONE 
     | SOME x' => f_opt x'
	
	
(* Problem 2.
* Write a function do_until : ('a -> 'a) -> ('a -> bool) -> 'a -> 'a. do_until f p x will apply f to x
* and f again to that result and so on until p x is false. Example: do_until (fn x => x div 2) (fn x => x mod 2 <> 1)
* will evaluate to a function of type int->int that divides its argument by 2 until it reaches an odd number. 
* In effect, it will remove all factors of 2 its argument.
*)

fun do_until f p = fn x => 
    case (p x) 
    of true  => do_until f p (f x)
     | false => x  
	

(* Problem 3.
* Use do_until to implement factorial.
*)

fun fact n =
    let val (ans,_) = do_until (fn (acc,m) => (acc*m,m-1)) (fn (_,m) => m <> 1) (1,n) 
    in ans 
    end 

fun fact' n = (* for testing *)
    if n = 1 then 1
    else n * fact' (n - 1)

	
(* Problem 4. 
 * Use do_until to write a function fixed_point: (''a -> ''a) -> ''a -> ''a that given a function f 
 * and an initial value x applies f to x until f x = x. (Notice the use of '' to indicate equality types.)
 *)	
 
fun fixed_point f x =
    case (f x = x)
    of true => x
     | false => fixed_point f (f x)  	
 
(* Problem 5.
 * Write a function map2 : ('a -> 'b) -> 'a * 'a -> 'b * 'b that given a function that takes ’a values to ’b
 * values and a pair of ’a values returns the corresponding pair of ’b values.
 *)
 
fun map2 f [] = []
  | map2 f ((x,y)::ls) = (f x, f y)::(map2 f ls)
  
  
(* Problem 6.
 * Write a function app_all : ('b -> 'c list) -> ('a -> 'b list) -> 'a -> 'c list, so that: 
 * app_all f g x will apply f to every element of the list g x and concatenate the results 
 * into a single list. For example, for fun f n = [n, 2 * n, 3 * n], we have 
 * app_all f f 1 = [1, 2, 3, 2, 4, 6, 3, 6, 9].
 *)
fun concat [] = []
  | concat (xs::xss) = xs@(concat xss) 
 
fun app_all f g x = concat (map f (g x))
 	
  
(* Problem 7.
 * Implement List.foldr
 * f init [x1, x2, ..., xn] returns f(x1, f(x2, ..., f(xn, init)...))
 * or init if the list is empty.
 *)
fun foldr _ v [] = v 
  | foldr f v (x::xs) = f x (foldr f v xs)  
	
