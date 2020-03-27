(* Author  : Pieter van Wyk
 * Created : 2020-03-27
 * Updated : 2020-03-27
 *
 * Solutions to the extra practice problems of week 2 of part A 
 *)
		
(* 1. a function that takes a list of numbers and adds them with alternating sign *)
fun alternate (ls: int list) : int =
    if null ls then 0
    else (hd ls) - alternate (tl ls)
	
(* 2. a function that takes a non-empty list of numbers, and returns a pair of 
      the minimum and maximum of the numbers in the list.*)
	  
(* helper function : generic function for defining maxInLst and minInLst *)
fun predInLst' (v : int)  (ls : int list) (p : int -> int -> bool) : int =	  
	if null ls then v
	else if (p (hd ls) v) then predInLst' v (tl ls) p
	     else predInLst' (hd ls) (tl ls) p 
fun predInLst (ls : int list) (p : int -> int -> bool) : int =	
	predInLst' (hd ls)	(tl ls) p	
	  
(* helper function : get maximum element in list *)	  
fun maxInLst (ls: int list) : int =
	predInLst ls (fn x => fn y => x <= y)
	
(* helper function : get minimum element in list *)
fun minInLst (ls: int list) : int =
	predInLst ls (fn x => fn y => x >= y)
	
(* main function *)	
fun min_max (ls: int list) : int*int =
	if length ls = 1 then (hd ls, hd ls)
	else (minInLst ls, maxInLst ls)

   
(* 3. a function that takes a list of numbers and returns a list of the partial sums of those numbers.*)

(* helper function : computes the sum of a list of integers *)
fun sumLst (ls : int list) : int =
	if null ls then 0
	else (hd ls) + sumLst (tl ls)

(* helper function : given list of lists of integers, computes list of sums *)
fun sumLstLst (lss:(int list) list) : int list =
	if null lss then []
	else (sumLst (hd lss))::(sumLstLst (tl lss)) 
