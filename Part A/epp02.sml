(* Author  : Pieter van Wyk
 * Created : 2020-04-13
 * Updated : 2020-04-13
 *
 * Solutions to the extra practice problems of week 3 of part A 
 *)
 
(* datatype for problem 1 ~ 4 *)
type student_id = int
type grade = int (* must be in 0 to 100 range *)
type final_grade = { id : student_id, grade : grade option }
datatype pass_fail = pass | fail


(* Problem 1. 
 * Write a function pass_or_fail of type {grade : int option, id : 'a} -> pass_fail 
 * that takes a final_grade (or, as the type indicates, a more general type) and 
 * returns pass if the grade field contains SOME i for an i≥75 (else fail).
 *)
 
fun pass_or_fail {grade : int option, id : 'a} : pass_fail =
    case grade 
    of NONE  => fail
    | SOME i => case (i >= 75) 
	        of true  => pass
		|  false => fail
				
(* Problem 2. 
 * Using pass_or_fail as a helper function, write a function has_passed of type 
 * {grade : int option, id : 'a} -> bool that returns true if and only if the the 
 * grade field contains SOME i for an i≥75.
 *)
 
 fun has_passed (x_rec : {grade : int option, id : 'a}) : bool =
     case (pass_or_fail x_rec)
     of pass => true
     |  fail => false
