(* Author  : Pieter van Wyk
 * Created : 2020-04-11
 * Updated : 2020-04-11
 *
 * Tests for the extra practice problems of week 3 of part A 
 *)
use "epp02.sml";

(* Problem 1. unit tests *)
val test_pass_or_fail1 = pass_or_fail {grade = NONE, id = 123} = fail 
val test_pass_or_fail2 = pass_or_fail {grade = SOME 50, id = 312} = fail
val test_pass_or_fail3 = pass_or_fail {grade = SOME 74, id = 231} = fail
val test_pass_or_fail4 = pass_or_fail {grade = SOME 75, id = 123} = pass 


(* Problem 2. unit tests *)
val test_has_passed1 = has_passed {grade = NONE, id = 123} = false 
val test_has_passed2 = has_passed {grade = SOME 80, id = 312} = true 
val test_has_passed3 = has_passed {grade = SOME 60, id = 231} = false 
val test_has_passed4 = has_passed {grade = SOME 75, id = 123} = true 