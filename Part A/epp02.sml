(* Author  : Pieter van Wyk
 * Created : 2020-04-13
 * Updated : 2020-04-18
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
 
fun pass_or_fail {id = _, grade = grd} =
    case grd 
	of NONE  => fail
	 | SOME i => case (i >= 75) 
	             of true  => pass
		      | false => fail
				
(* Problem 2. 
 * Using pass_or_fail as a helper function, write a function has_passed of type 
 * {grade : int option, id : 'a} -> bool that returns true if and only if the the 
 * grade field contains SOME i for an i≥75.
 *)
 
 fun has_passed (fgrd : final_grade) : bool =
     case (pass_or_fail fgrd)
     of pass => true
     |  fail => false

(* Problem 3. 
 * Using has_passed as a helper function, write a function number_passed that takes 
 * a list of type final_grade (or a more general type) and returns how many list 
 * elements have passing (again, ≥75) grades.
 *)
 
fun number_passed (fgrd_ls : final_grade list) : int =
    case fgrd_ls
    of [] => 0
    | (fgrd::fgrd_ls') => case (has_passed fgrd) 
	                  of true => 1 + number_passed fgrd_ls' 
	                  |  false => number_passed fgrd_ls' 
 
(* Problem 4. 
 * Write a function number_misgraded of type (pass_fail * final_grade) list -> int
 * that indicates how many list elements are "mislabeled" where mislabeling means a 
 * pair (pass,x) where has_passed x is false or (fail,x) where has_passed x is true.
 *) 
 
fun number_misgraded (check_ls : (pass_fail * final_grade) list) : int =
    case check_ls
    of [] => 0
    | (check::check_ls') => let val grade_given = #1 check 
	                        val grade_actual = pass_or_fail (#2 check) 
	                    in case (grade_given = grade_actual)
			       of true  => number_misgraded check_ls'
			       |  false => 1 + number_misgraded check_ls'
			    end
