(* Author  : Pieter van Wyk
 * Created : 2020-04-11
 * Updated : 2020-04-11
 *
 * Tests for the extra practice problems of week 2 of part A 
 *)
use "epp01.sml";

(* Problem 1. unit tests *)
val test_alternate1 = alternate [1,2,3,4,0] = ~2
val test_alternate2 = alternate [0,1,2,3,4] = 2	
val test_alternate3 = alternate [1,~2,3,~4] = 10
val test_alternate4 = alternate [1,2,3,4,5] = 3	


(* Problem 2. unit tests *)
val test_min_max1 = min_max [1,2,3,4,0] = (0,4)
val test_min_max2 = min_max [0,1,2,3,4] = (0,4)
val test_min_max3 = min_max [~3,1,5,3,4] = (~3,5)
val test_min_max4 = min_max [~1,~2,~3,~4,0] = (~4,0)


(* Problem 3. unit tests *)
val test_cumsum1 = cumsum [~1,~2,~3,~4,0] = [~1,~3,~6,~10,~10]
val test_cumsum2 = cumsum [1,~1,2,~2] = [1,0,2,0]
val test_cumsum3 = cumsum [5,~1,~2,~2] = [5,4,2,0]
val test_cumsum4 = cumsum [1,4,5,3] = [1,5,10,13]


(* Problem 4. unit tests *)
val test_greeting1 = greeting (SOME "Old Greg") = "Hello there, Old Greg!"
val test_greeting2 = greeting NONE = "Hello there, you!"
val test_greeting3 = greeting (SOME "Michelle") = "Hello there, Michelle!"
val test_greeting4 = greeting (SOME "asfosh90") = "Hello there, asfosh90!"


(* Problem 5. unit tests *)
val test_repeat1 = repeat ([1,2,3],[1,2,3]) = [1,2,2,3,3,3]
val test_repeat2 = repeat ([3,2,1],[1,2,3]) = [3,2,2,1,1,1]
val test_repeat3 = repeat ([1,2],[1,2,3]) = [1,2,2]
val test_repeat4 = repeat ([1,2,3],[1,2]) = [1,2,2]


(* Problem 6. unit tests *)
val test_addOpt1 = addOpt NONE (SOME 3) = NONE
val test_addOpt2 = addOpt (SOME 2) NONE = NONE
val test_addOpt3 = addOpt (SOME 2) (SOME 3) = SOME 5
val test_addOpt4 = addOpt (SOME 2) (SOME ~2) = SOME 0
  
  
(* Problem 7. unit tests *)
val test_addAllOpt1 = addAllOpt [NONE, NONE, NONE] = NONE
val test_addAllOpt2 = addAllOpt [SOME 1, NONE, NONE] = SOME 1
val test_addAllOpt3 = addAllOpt [SOME 1, SOME 2, NONE] = SOME 3
val test_addAllOpt4 = addAllOpt [SOME 1, SOME 2, SOME 3] = SOME 6
  
  
(* Problem 8. unit tests *)  
val test_any1 = any [false, false, false] = false
val test_any2 = any [true, false, false] = true
val test_any3 = any [false, true, false] = true
val test_any4 = any [false, false, true] = true


(* Problem 9. unit tests *) 
val test_all1 = all [true, true, true] = true
val test_all2 = all [true, true, false] = false
val test_all3 = all [true, false, true] = false
val test_all4 = all [false, true, true] = false
 
 
(* Problem 10. unit tests *)  
val test_zip1 = zip [1,2,3] ["a","b","c"] = [(1,"a"),(2,"b"),(3,"c")] 
val test_zip2 = zip [1,2] ["a","b","c"] = [(1,"a"),(2,"b")] 
val test_zip3 = zip [1,2,3] ["a","b"] = [(1,"a"),(2,"b")] 
val test_zip4 = zip ["a","b","c"] [1,2,3] = [("a",1),("b",2),("c",3)]  
 
 
(* Problem 11. unit tests *) 
val test_zipRecycle1 = zipRecycle [1,2,3] ["a","b","c"] = [(1,"a"),(2,"b"),(3,"c")]  
val test_zipRecycle2 = zipRecycle [1,2,3] ["a","b"] = [(1,"a"),(2,"b"),(3,"a")] 
val test_zipRecycle3 = zipRecycle [1,2] ["a","b","c"] = [(1,"a"),(2,"b"),(1,"c")]  
val test_zipRecycle4 = zipRecycle [1] ["a","b","c"] = [(1,"a"),(1,"b"),(1,"c")] 
 

(* Problem 12. unit tests *) 
val test_zipOpt1 = zipOpt [1,2,3] ["a","b","c"] = SOME [(1,"a"),(2,"b"),(3,"c")] 
val test_zipOpt2 = zipOpt [1,2] ["a","b","c"] = NONE
val test_zipOpt3 = zipOpt [1,2,3] ["a","b"] = NONE
val test_zipOpt4 = zipOpt [1,2] ["a","b"] = SOME [(1,"a"),(2,"b")] 


(* Problem 13. unit tests *) 
val test_lookup1 = lookup [("a",1),("b",2),("c",3)] "a" = SOME 1
val test_lookup2 = lookup [("a",1),("b",2),("c",3)] "b" = SOME 2
val test_lookup3 = lookup [("a",1),("b",2),("c",3)] "c" = SOME 3
val test_lookup4 = lookup [("a",1),("b",2),("c",3)] "d" = NONE


(* Problem 14. unit tests *) 
val test_splitup1 = splitup [1,2,3,4,5] = ([1,2,3,4,5],[])
val test_splitup2 = splitup [1,~2,3,~4,5] = ([1,3,5],[~2,~4])
val test_splitup3 = splitup [~1,2,~3,4,~5] = ([2,4],[~1,~3,~5])
val test_splitup4 = splitup [~1,~2,~3,~4,~5] = ([],[~1,~2,~3,~4,~5])


(* Problem 15. unit tests *) 
val test_splitAt1 = splitAt [1,2,3,4,5] 6 = ([1,2,3,4,5],[]) 
val test_splitAt2 = splitAt [1,2,3,4,5] 5 = ([1,2,3,4],[5]) 
val test_splitAt3 = splitAt [1,2,3,4,5] 4 = ([1,2,3],[4,5]) 
val test_splitAt4 = splitAt [1,2,3,4,5] 3 = ([1,2],[3,4,5]) 


(* Problem 16. unit tests *) 
val test_isSorted1 = isSorted [1,2,3,4,5] = true
val test_isSorted2 = isSorted [5,4,3,2,1] = false
val test_isSorted3 = isSorted [1,4,5,7,9] = true
val test_isSorted4 = isSorted [1,4,3,7,9] = false


(* Problem 17. unit tests *) 
val test_isAnySorted1 = isAnySorted [1,2,3,4,5] = true
val test_isAnySorted2 = isAnySorted [5,4,3,2,1] = true
val test_isAnySorted3 = isAnySorted [1,4,5,7,9] = true
val test_isAnySorted4 = isAnySorted [1,4,3,7,9] = false 


(* Problem 18. unit tests *) 
val test_sortedMerge1 = isSorted (sortedMerge [1,3,5] [~2,~1]) 
val test_sortedMerge2 = isSorted (sortedMerge [~2,~1] [1,3,5])
val test_sortedMerge3 = isSorted (sortedMerge [~1,3,5] [~2,1])
val test_sortedMerge4 = isSorted (sortedMerge [~5,1,3] [~2,5])


(* Problem 19. unit tests *) 
val test_qsort1 = isSorted (qsort [1,3,5,~2,~1]) 
val test_qsort2 = isSorted (qsort [~2,~1,1,3,5]) 
val test_qsort3 = isSorted (qsort [1,3,5,~2,~1]) 
val test_qsort4 = isSorted (qsort [~5,1,3,~2,5]) 
 

(* Problem 20. unit tests *) 
val test_divide1 = divide ([1,2,3,4,5,6,7]) = ([1,3,5,7], [2,4,6]) 
val test_divide2 = divide ([2,3,4,5,6,7,8]) = ([2,4,6,8], [3,5,7]) 
val test_divide3 = divide ([3,4,5,6,7,8,9]) = ([3,5,7,9], [4,6,8]) 
val test_divide4 = divide ([4,5,6,7,8,9,1]) = ([4,6,8,1], [5,7,9]) 


(* Problem 21. unit tests *) 
val test_not_so_quick_sort1 = isSorted (not_so_quick_sort [1,3,5,~2,~1]) 
val test_not_so_quick_sort2 = isSorted (not_so_quick_sort [~2,~1,1,3,5]) 
val test_not_so_quick_sort3 = isSorted (not_so_quick_sort [1,3,5,~2,~1]) 
val test_not_so_quick_sort4 = isSorted (not_so_quick_sort [~5,1,3,~2,5]) 
