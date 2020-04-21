(* Author  : Pieter van Wyk
 * Created : 2020-04-11
 * Updated : 2020-04-21
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


(* test data for problem 5 ~ 7 *)
val t1 = node {value = 3, left = leaf, right = leaf}
val t2 = node {value = 7, left = leaf, right = t1}
val t3 = node {value = 4, left = t1, right = t2}
val t4 = node {value = 2, left = t2, right = t3}

(* Problem 5. unit tests *)
val test_tree_height1 = tree_height t1 = 1
val test_tree_height2 = tree_height t2 = 2
val test_tree_height3 = tree_height t3 = 3
val test_tree_height4 = tree_height t4 = 4


(* Problem 6. unit tests *)
val test_sum_tree1 = sum_tree t1 = 3
val test_sum_tree2 = sum_tree t2 = 10 
val test_sum_tree3 = sum_tree t3 = 17 
val test_sum_tree4 = sum_tree t4 = 29 


(* Problem 7. unit tests *)
val t1' = node {value = prune_me, left = leaf, right = leaf}
val t2' = node {value = leave_me_alone, left = leaf, right = t1'}  
val t3' = node {value = leave_me_alone, left = t1', right = t2'}
val t4' = node {value = leave_me_alone, left = t2', right = t1'}

val test_gardener1 = gardener t1' = leaf 
val test_gardener2 = gardener t2' = node {value = leave_me_alone, left = leaf, right = leaf} 
val test_gardener3 = gardener t3' = node {value = leave_me_alone, left = leaf, right = node {value = leave_me_alone, left = leaf, right = leaf}}
val test_gardener4 = gardener t4' = node {value = leave_me_alone, left = node {value = leave_me_alone, left = leaf, right = leaf}, right = leaf}


(* Problem 8. unit tests *)
val test_myLast1 = ((myLast []) handle Error => 42) = 42 
val test_myLast2 = myLast [3] = 3
val test_myLast3 = myLast ["a","b","c","d"] = "d"

val test_myTake1 = ((myTake 1 []) handle Error => [42]) = [42]  
val test_myTake2 = myTake 3 [1,2,3,4] = [1,2,3]
val test_myTake3 = myTake 2 [1,2,3,4] = [1,2] 

val test_myDrop1 = ((myDrop 1 []) handle Error => [42]) = [42]  
val test_myDrop2 = myDrop 3 [1,2,3,4] = [4]
val test_myDrop3 = myDrop 2 [1,2,3,4] = [3,4] 

val take_drop = fn n => (fn xs => (myTake n xs)@(myDrop n xs))
val test_take_drop1 = take_drop 3 [1,2,5,5,2,3] = [1,2,5,5,2,3]  
val test_take_drop2 = take_drop 4 [3,2,4,7,8,9] = [3,2,4,7,8,9] 
val test_take_drop3 = take_drop 2 [5,6,2,9,3,8] = [5,6,2,9,3,8]  
val test_take_drop4 = take_drop 5 [8,1,3,4,5,5] = [8,1,3,4,5,5] 

val test_concat1 = myConcat [[1,1],[2,3,1],[2,3,4,5]] = [1,1,2,3,1,2,3,4,5] 
val test_concat2 = myConcat [[5,2],[3,4,2],[4,1,1,4]] = [5,2,3,4,2,4,1,1,4]
val test_concat3 = myConcat [[9,8],[4,7,5],[1,2,3,1]] = [9,8,4,7,5,1,2,3,1]

val test_getOpt1 = myGetOpt (NONE,#"3") = #"3"
val test_getOpt2 = myGetOpt (SOME 4,3) = 4
val test_getOpt3 = myGetOpt (SOME "hello","goodby") = "hello"

val test_join1 = myJoin NONE = NONE 
val test_join2 = myJoin (SOME (SOME 3)) = SOME 3 
val test_join3 = myJoin (SOME (SOME (SOME "a"))) = SOME (SOME "a")


(* test data for problem 9 ~ 16 *)
val one = SUCC ZERO
val two = SUCC one 
val three = SUCC two 
val four = SUCC three 
val five = SUCC four 
val six = SUCC five 

(* Problem 9. unit tests *)
val test_is_positive1 = is_positive ZERO = false 
val test_is_positive2 = is_positive one = true 
val test_is_positive3 = is_positive two = true 
val test_is_positive4 = is_positive three = true 


(* Problem 10. unit tests *)
val test_myPred1 = ((myPred ZERO) handle Negative => ZERO) = ZERO 
val test_myPred2 = myPred one = ZERO 
val test_myPred3 = myPred two = one 
val test_myPred4 = myPred three = two  


(* Problem 11. unit tests *)
val test_nat_to_int1 = nat_to_int six = 6
val test_nat_to_int2 = nat_to_int five = 5
val test_nat_to_int3 = nat_to_int four = 4
val test_nat_to_int4 = nat_to_int three = 3


(* Problem 12. unit tests *)
val test_int_to_nat1 = ((int_to_nat (~1)) handle Negative => ZERO) = ZERO
val test_int_to_nat2 = int_to_nat 2 = two
val test_int_to_nat3 = int_to_nat 3 = three
val test_int_to_nat4 = int_to_nat 4 = four


(* Problem 13. unit tests *)
val test_add1 = add (one,one) = two
val test_add2 = add (two,three) = five
val test_add3 = add (two,four) = six
val test_add4 = add (one,five) = six


(* Problem 14. unit tests *)
val test_sub1 = (sub (one,six) handle Negative => ZERO) = ZERO 
val test_sub2 = sub (six,one) = five 
val test_sub3 = sub (five,two) = three  
val test_sub4 = sub (four,four) = ZERO 


(* Problem 15. unit tests *)
val test_mult1 = mult (four,one)  = add (two,two)
val test_mult2 = mult (three,two) = add (four,two)
val test_mult3 = mult (two,two) = sub (six,two)
val test_mult4 = mult (one,five) = sub (six,one)


(* Problem 16. unit tests *)
val test_less_than1 = less_than (two,one) = false 
val test_less_than2 = less_than (five,six) = true
val test_less_than3 = less_than (three,three) = false 
val test_less_than4 = less_than (two,five) = true  


(* test data for problem 17 ~ 19 *)
val set1 = Elems []
val set2 = Elems [1,2]
val set3 = Elems [3,4]
val set4 = Elems [2,5]

(* Problem 17. unit tests *)
val test_isEmpty1 = isEmpty set1 = true
val test_isEmpty2 = isEmpty set2 = false
val test_isEmpty3 = isEmpty (Range {from = 4, to = 3}) = true    
val test_isEmpty4 = isEmpty (Range {from = 3, to = 4}) = false  
val test_isEmpty5 = isEmpty (Union (set1,set1)) = true 
val test_isEmpty6 = isEmpty (Union (set1,set2)) = false   
val test_isEmpty7 = isEmpty (Intersection (set1,set2)) = true 
val test_isEmpty8 = isEmpty (Intersection (set3,set2)) = false  

(* Problem 18. unit tests *)
val test_isElm1 = isElm 3 [1,2,3] = true 
val test_isElm2 = isElm 5 [1,2,3] = false
val test_isElm3 = isElm "a" ["a","b","c"] = true 
val test_isElm4 = isElm "d" ["a","b","c"] = false

val test_contains1 = contains (set2, 2) = true
val test_contains2 = contains (set3, 5) = false 
val test_contains3 = contains (Range {from = 1, to = 3},2) = true    
val test_contains4 = contains (Range {from = 3, to = 5},1) = false  
val test_contains5 = contains (Union (set1,set2),2) = true 
val test_contains6 = contains (Union (set2,set3),5) = false   
val test_contains7 = contains (Intersection (set2,set4),2) = true 
val test_contains8 = contains (Intersection (set2,set4),5) = false 


(* Problem 19. unit tests *)
val test_removeElm1 = removeElm 3 [3,3,3,3] = []
val test_removeElm2 = removeElm 2 [1,2,3,2] = [1,3]
val test_removeElm3 = removeElm 5 [1,2,3,4] = [1,2,3,4]
val test_removeElm4 = removeElm "b" ["a","b","c"] = ["a","c"]

val test_removeDup1 = removeDup [3,3,3,3] = [3]
val test_removeDup2 = removeDup [1,2,2,1] = [1,2]
val test_removeDup3 = removeDup ["a","b","b","a"] = ["a","b"]
val test_removeDup4 = removeDup [1,2,3,4] = [1,2,3,4]

val test_rangeToList1 = rangeToList 1 3 = [1,2,3]
val test_rangeToList2 = rangeToList 5 8 = [5,6,7,8]
val test_rangeToList3 = rangeToList 5 4 = []
val test_rangeToList4 = rangeToList 5 5 = [5]

val test_intersectionToList1 = intersectionToList [] [1] = []
val test_intersectionToList2 = intersectionToList [1] [] = []
val test_intersectionToList3 = intersectionToList [1,2,3,3] [2,2,3,4] = [2,3,3] 
val test_intersectionToList4 = intersectionToList [1,2,3] [4,5] = []

val test_toList1 = toList set2 = [1,2]
val test_toList2 = toList set3 = [3,4] 
val test_toList3 = toList (Range {from = 1, to = 3}) = [1,2,3]    
val test_toList4 = toList (Range {from = 3, to = 5}) = [3,4,5]  
val test_toList5 = toList (Union (set1,set2)) = [1,2] 
val test_toList6 = toList (Union (set2,set3)) = [1,2,3,4]   
val test_toList7 = toList (Intersection (set2,set4)) = [2] 
val test_toList8 = toList (Intersection (set2,set3)) = [] 

(* END *)
