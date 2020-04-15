(* Author  : Pieter van Wyk
 * Created : 2020-04-11
 * Updated : 2020-04-15
 *
 * Tests for the extra practice problems of week 3 of part A 
 *)
use "epp02.sml";

(* test data for problem 1 ~ 4 *)
val fgrd1 = {grade = NONE, id = 123}    (* fail *)
val fgrd2 = {grade = SOME 50, id = 312} (* fail *)
val fgrd3 = {grade = SOME 74, id = 231} (* fail *)
val fgrd4 = {grade = SOME 75, id = 123} (* pass *)
val fgrd5 = {grade = SOME 80, id = 312} (* pass *)
val fgrd6 = {grade = SOME 60, id = 231} (* fail *)


(* Problem 1. unit tests *)
val test_pass_or_fail1 = pass_or_fail fgrd1 = fail 
val test_pass_or_fail2 = pass_or_fail fgrd2 = fail
val test_pass_or_fail3 = pass_or_fail fgrd3 = fail
val test_pass_or_fail4 = pass_or_fail fgrd4 = pass 


(* Problem 2. unit tests *)
val test_has_passed1 = has_passed fgrd1 = false 
val test_has_passed2 = has_passed fgrd5 = true 
val test_has_passed3 = has_passed fgrd6 = false 
val test_has_passed4 = has_passed fgrd4 = true 


(* Problem 3. unit tests *)
val test_number_passed1 = number_passed [fgrd1,fgrd2,fgrd3,fgrd4] = 1
val test_number_passed2 = number_passed [fgrd2,fgrd3,fgrd4,fgrd5] = 2
val test_number_passed3 = number_passed [fgrd3,fgrd4,fgrd5,fgrd6] = 2
val test_number_passed4 = number_passed [fgrd4,fgrd5,fgrd6,fgrd1] = 2


(* Problem 4. unit tests *)
val test_number_misgraded1 = number_misgraded [(fail,fgrd1),(fail,fgrd2),(fail,fgrd3),(pass,fgrd4)] = 0
val test_number_misgraded2 = number_misgraded [(pass,fgrd1),(fail,fgrd2),(fail,fgrd3),(pass,fgrd4)] = 1
val test_number_misgraded3 = number_misgraded [(pass,fgrd1),(pass,fgrd2),(fail,fgrd3),(pass,fgrd4)] = 2
val test_number_misgraded4 = number_misgraded [(pass,fgrd1),(pass,fgrd2),(fail,fgrd3),(fail,fgrd4)] = 3
