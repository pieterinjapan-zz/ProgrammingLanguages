(* Author  : Pieter van Wyk
 * Created : 2020-04-13
 * Updated : 2020-04-20
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


(* datatype for problem 5 ~ 7 *)
datatype 'a tree = leaf 
                 | node of { value : 'a, left : 'a tree, right : 'a tree }
datatype flag = leave_me_alone | prune_me

(* Problem 5. 
 * Write a function tree_height that accepts an 'a tree and evaluates to a 
 * height of this tree. The height of a tree is the length of the longest 
 * path to a leaf. Thus the height of a leaf is 0.
 *) 
 
fun tree_height tree = 
    case tree
    of leaf => 0
     | node {value = _, left = treeL, right = treeR } => let val heightL = tree_height treeL
	                                                     val heightR = tree_height treeR
							 in 1 + (if (heightL > heightR) then heightL else heightR) 	
							 end
	
	
(* Problem 6. 
 * Write a function sum_tree that takes an int tree and 
 * evaluates to the sum of all values in the nodes.
 *) 
 
fun sum_tree tree =
    case tree
    of leaf => 0
     | node {value = x, left = treeL, right = treeR } => let val sumL = sum_tree treeL
	                                                     val sumR = sum_tree treeR
					  		 in x + sumL + sumR	
							 end


(* Problem 7. 
 * Write a function gardener of type flag tree -> flag tree such that 
 * its structure is identical to the original tree except all nodes of 
 * the input containing prune_me are (along with all their descendants) 
 * replaced with a leaf.
 *) 

fun gardener tree =
    case tree 
	of leaf => leaf
	 | node {value = prune_me, left = treeL, right = treeR } => leaf
	 | node {value = leave_me_alone, left = treeL, right = treeR } => let val treeL' = gardener treeL
                                                                              val treeR' = gardener treeR	 
									  in node {value = leave_me_alone, left = treeL', right = treeR' }
                                                                          end																	  


(* Problem 8. 
 * Re-implement various functions provided in the SML standard libraries for lists and options. 
 * See http://sml-family.org/Basis/list.html and http://sml-family.org/Basis/option.html. 
 * Good examples include last, take, drop, concat, getOpt, and join.
 *) 
 
(* last : returns the last element of l. *)
exception Error

fun myLast xs =
    case xs 
    of []  => raise Error
     | [x] => x
     | _::xs' => myLast xs'	 

(* take : returns the first i elements of the list l. *)
fun myTake 0 xs = []
  | myTake n [] = raise Error 
  | myTake n (x::xs) = x::(myTake (n - 1) xs)  

(* drop : returns what is left after dropping the first i elements of the list l. *)
fun myDrop 0 xs = xs 
  | myDrop n [] = raise Error
  | myDrop n (_::xs) = myDrop (n-1) xs

(* concat : returns the list that is the concatenation of all the lists in l in order. *)
fun myConcat [] = []
  | myConcat (xs::xss) = xs@(myConcat xss)
	
(* getOpt : returns v if opt is SOME v; otherwise it returns a. *)
fun myGetOpt (NONE,x)   = x 
  | myGetOpt (SOME i,_) = i  

(* join : The join function maps NONE to NONE and SOME v to v. *)
fun myJoin NONE = NONE
  | myJoin (SOME x) = x 
  
  
(* datatype for problem 1 ~ 4 *)
datatype nat = ZERO | SUCC of nat

(* Problem 9. 
 * Write is_positive : nat -> bool, which given a "natural number" 
 * returns whether that number is positive (i.e. not zero).
 *) 
 
fun is_positive ZERO = false 
  | is_positive _    = true 
  
 
(* Problem 10. 
 * Write pred : nat -> nat, which given a "natural number" returns its predecessor. 
 * Since ZERO does not have a predecessor in the natural numbers, throw an exception Negative.
 *) 
exception Negative 

fun myPred ZERO = raise Negative
  | myPred (SUCC n) = n  


(* Problem 11. 
 * Write nat_to_int : nat -> int, which given a "natural number" returns the corresponding int. 
 * For example, nat_to_int (SUCC (SUCC ZERO)) = 2. (Do not use this function for problems 13-16.)
 *)
 
fun nat_to_int ZERO = 0
  | nat_to_int (SUCC n) = 1 + nat_to_int n 
 

(* Problem 12. 
 * Write int_to_nat : int -> nat which given an integer returns a "natural number" representation 
 * for it, or throws a Negative exception if the integer was negative. 
 * (Do not use this function for problems 13-16.)
 *)
 
fun int_to_nat 0 = ZERO 
  | int_to_nat n = case (n > 0)
                   of true => SUCC (int_to_nat (n - 1))
		    | false => raise Negative
 
 
(* Problem 13. 
 * Write add : nat * nat -> nat to perform addition.
 *)
 
fun add (ZERO,m) = m 
  | add (SUCC n,m) = SUCC (add (n,m))


(* Problem 14. 
 * Write sub : nat * nat -> nat to perform subtraction. 
 *)
 
fun sub (n,ZERO) = n 
  | sub (ZERO,SUCC m) = raise Negative
  | sub (SUCC n,SUCC m) = sub (n,m)  
 

(* Problem 15. 
 * Write mult : nat * nat -> nat to perform multiplication. 
*)


fun mult (ZERO,_) = ZERO 
  | mult (_,ZERO) = ZERO 
  | mult (SUCC n,m) = add (mult(n,m),m)


(* Problem 16. 
 * Write less_than : nat * nat -> bool to return true when
 * the first argument is less than the second.
 *)
 
fun less_than (ZERO,m) = (case m 
                          of ZERO => false 
                           | _ => true)
  | less_than (SUCC n',m) = (case m 
	                     of ZERO => false 
                              | SUCC m' => less_than (n',m'))						  
    	 

(* datatype for problem 17 ~ 19 *)
datatype intSet = 
  Elems of int list (*list of integers, possibly with duplicates to be ignored*)
| Range of { from : int, to : int }  (* integers from one number to another *)
| Union of intSet * intSet (* union of the two sets *)
| Intersection of intSet * intSet (* intersection of the two sets *)

(* Problem 17. 
 * Write isEmpty : intSet -> bool that determines if the set is empty or not.
 *)
 
fun isEmpty set = 
    case set 
    of Elems ls => (ls = [])
     | Range {from = x_i, to = x_f } => (x_i > x_f)	
     | Union (set1,set2) => (isEmpty set1) andalso (isEmpty set2)
     | Intersection (set1,set2) => (isEmpty set1) orelse (isEmpty set2)
