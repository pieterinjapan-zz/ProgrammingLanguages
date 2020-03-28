(* Author  : Pieter van Wyk
 * Created : 2020-03-27
 * Updated : 2020-03-28
 *
 * Solutions to the extra practice problems of week 2 of part A 
 *)
		
(* Problem 1. 
 * a function that takes a list of numbers and 
 * adds them with alternating sign 
 *)
 
fun alternate (ls: int list) : int =
    if null ls then 0
    else (hd ls) - alternate (tl ls)
	
(* Problem 2. 
 * a function that takes a non-empty list of numbers, 
 * and returns a pair of the minimum and maximum of 
 * the numbers in the list 
 *)
	  
(* helper function : generic function for defining maxInLst and minInLst *)	
fun predInLst (ls : int list) (p : int -> int -> bool) : int =
	let fun predInLst' (v : int)  (ls : int list) (p : int -> int -> bool) : int =	  
		  if null ls then v
	      else if (p (hd ls) v) then predInLst' v (tl ls) p
	      else predInLst' (hd ls) (tl ls) p
	in predInLst' (hd ls) (tl ls) p	
	end
	  	
(* main function *)	
fun min_max (ls: int list) : int*int =
	if length ls = 1 then (hd ls, hd ls)
	else let val minInLst = (fn xs => (predInLst ls (fn x => fn y => x >= y)))
	         val maxInLst = (fn xs => (predInLst ls (fn x => fn y => x <= y)))
	     in (minInLst ls, maxInLst ls)	
         end
		 
(* Problem 3. 
 * a function that takes a list of numbers and returns a list 
 * of the partial sums of those numbers.
 *)

(* helper function : computes the sum of a list of integers *)
fun sumLst (ls : int list) : int =
	if null ls then 0
	else (hd ls) + sumLst (tl ls)

(* helper function : given list of lists of integers, computes list of sums *)
fun sumLstLst (lss:(int list) list) : int list =
	if null lss then []
	else (sumLst (hd lss))::(sumLstLst (tl lss)) 
	
(* helper function : append two lists *)	
fun append (xs : 'a list) (ys : 'a list) : 'a list =
	if null xs then ys
	else (hd xs)::(append (tl xs) ys)
	
(* helper function : chop off last element from list *)
fun init (xs : 'a list)	: 'a list =
    if null xs then []
	else if length xs = 1 then []
	else (hd xs)::init (tl xs)

(* helper function : breaks list into partial lists *)
fun partLst (xs : 'a list) : ('a list) list =
	if null xs then []
	else if null (tl xs) then [[hd xs]]
	else append (partLst (init xs)) [xs]
	
(* main function *)	
fun cumsum (xs : int list) : int list = 
	sumLstLst (partLst xs)

(* Problem 4. 
 * a function that given a string option SOME name returns the string 
 * "Hello there, ...!" where the dots would be replaced by name. Note that 
 * the name is given as an option, so if it is NONE then replace the dots with "you".
 *)	

fun greeting (str_op : string option) : string =
    let val ht = "Hello there, "
	in if isSome str_op then ht ^ (valOf str_op) ^ "!"
	   else ht ^ "you!"
	end
	
(* Problem 5. 
 * a function that given a list of integers and another list of nonnegative 
 * integers, repeats the integers in the first list according to the numbers 
 * indicated by the second list.
 *)		

fun repeat (ps : (int list)*(int list)) : int list =
    let val xs = #1 ps
	    val ys = #2 ps
		fun repeatInt (n : int) (m : int) : int list =
            if n = 0 then []
	        else m::(repeatInt (n-1) m)
    in if null xs then []
	   else if null ys then []
	   else append (repeatInt (hd ys) (hd xs)) (repeat (tl xs,tl ys))
	end   

