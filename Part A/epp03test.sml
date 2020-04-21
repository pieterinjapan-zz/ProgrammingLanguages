(* Author  : Pieter van Wyk
 * Created : 2020-04-21
 * Updated : 2020-04-21
 *
 * Tests for solutions to the extra practice problems of week 4 of part A 
 *)
use "epp03.sml"; 


(* Problem 1. unit tests *)
fun f_opt1 xs = 
    case xs 
	of [] => NONE
	 | (x::_) => SOME x
	
fun f_opt2 xs =
    case xs
    of [] => NONE 
     | (_::xs) => SOME xs	
	
fun f_opt3 x =
    case (x < 0) 
	of true => NONE 
	 | false => SOME x
	
val test_compose_opt1 = (compose_opt f_opt1 f_opt2) [1,2,3] = SOME 2	
val test_compose_opt2 = (compose_opt f_opt3 f_opt1) [~1,2,3] = NONE 
val test_compose_opt3 = (compose_opt f_opt3 f_opt1) [1,2,3] = SOME 1 	
val test_compose_opt4 = (compose_opt f_opt3 (compose_opt f_opt1 (compose_opt f_opt2 f_opt2))) [5,4,3] = SOME 3


(* Problem 2. unit tests *)