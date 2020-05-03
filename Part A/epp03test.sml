(* Author  : Pieter van Wyk
 * Created : 2020-04-21
 * Updated : 2020-05-01
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
	 
val zeroOrNone = fn n => if n = 0 then SOME (SOME 0) else NONE

val someNOrNone = fn n => case n 
                          of SOME n => SOME n
                           | _      => NONE

val test_compose_opt1 = compose_opt f_opt1 f_opt2 [1,2,3] = SOME 2	
val test_compose_opt2 = compose_opt f_opt3 f_opt1 [~1,2,3] = NONE 
val test_compose_opt3 = compose_opt f_opt3 f_opt1 [1,2,3] = SOME 1 	
val test_compose_opt4 = compose_opt f_opt3 (compose_opt f_opt1 (compose_opt f_opt2 f_opt2)) [5,4,3] = SOME 3
val test_compose_opt5 = compose_opt someNOrNone zeroOrNone 0 = SOME 0
val test_compose_opt6 = compose_opt someNOrNone zeroOrNone 1 = NONE 


(* Problem 2. unit tests *)
val devideOutN = fn n => do_until (fn x => x div n) (fn x => x mod n = 0)
val devideOutTwo   = devideOutN 2 
val devideOutThree = devideOutN 3 

val test_do_until1  = devideOutTwo 8 = 1
val test_do_until2  = devideOutTwo 20 = 5 
val test_do_until3  = devideOutThree 9 = 1 
val test_do_until4  = devideOutN 9 27 = 3 
val test_do_until5  = do_until (fn x => x div 3) (fn x => x mod 3 = 0) 48 = 16
val test_do_until6  = do_until (fn x => x div 2) (fn x => x mod 2 <> 0) 9 = 4
val test_do_until7  = do_until (fn x => x ^ ".") (fn x => String.size x < 9) "abcde" = "abcde...."
val test_do_until8  = do_until (fn x => x div 2) (fn x => x mod 2 <> 1) 10 = 5
val test_do_until9  = do_until (fn x => x div 2) (fn x => x mod 2 <> 1) 6 = 3
val test_do_until10 = do_until (fn x => x div 2) (fn x => x mod 2 <> 1) 4 = 1
val test_do_until11 = do_until (fn x => x div 2) (fn x => x mod 2 <> 1) 5 = 5


(* Problem 3. unit tests *)
val test_fact = let val testls = map (fn n => fact n = fact' n) [0,1,2,3,4,5,6,7,8,9,10,11,12]
                in foldl (fn (x,acc) => acc andalso x) true testls 
				end 
				

(* Problem 4. unit tests *)	
val test_fixed_point1 = fixed_point (fn n => n div 5) 25 = 0
val test_fixed_point2 = fixed_point (fn n => n div 3) 64 = 0
val test_fixed_point3 = fixed_point (fn n => n div 7) 17 = 0
val test_fixed_point4 = fixed_point (fn n => n div 8) 98 = 0  
val test_fixed_point5 = fixed_point (fn x => x) 17 = 17 	


(* Problem 5. unit tests *)
val test_map2_1 = map2 String.size ("abc","abcd") = (3,4)
val test_map2_2 = map2 hd ([4,5],[6]) = (4,6)
val test_map2_3 = map2 tl ([4,5],[6]) = ([5],[])
val test_map2_4 = map2 (fn n => n mod 2 = 0) (3,4) = (false,true)
val test_map2_5 = map2 (fn x => ~x) (1,2) = (~1,~2)
val test_map2_6 = map2 (fn x => x * x) (1,2) = (1,4)


(* Problem 6. unit tests *)
fun f1 n = [n,2*n,3*n]
fun f2 n = [n*2,n*3]

val test_app_all1 = app_all f1 f1 1 = [1,2,3,2,4,6,3,6,9]
   
val test_app_all2 =
    let fun f s = [s^"_1", s^"_2"]
	in app_all f f "test" = ["test_1_1","test_1_2","test_2_1","test_2_2"]
	end 
	
val test_app_all3 =
    let fun g s = [s, s^s]
	    fun f s = [String.size s,3]
	in app_all f g "test" = [4,3,8,3]
	end
	
val test_app_all4 =
    let fun g n = [devideOutTwo n, devideOutThree n]
	in app_all f2 g 24 = [6,9,16,24] 
	end		

val test_app_all5 = app_all f1 f2 2 = [4,8,12,6,12,18]


(* Problem 7. unit tests *)	
val test_foldr1 = foldr (fn x => fn acc => x + acc) 0 [1,2,3,4] = List.foldr (fn (x, acc) => x + acc) 0 [1,2,3,4]
val test_foldr2 = foldr (fn x => fn v => x * v) 1 [1,2,3,4] = List.foldr (fn (x,v) => x * v) 1 [1,2,3,4] 
val test_foldr3 = foldr (fn x => fn v => (String.size x) + v) 0 ["1","2","3","4"] = List.foldr (fn (x,v) => (String.size x) + v) 0 ["1","2","3","4"] 
val test_foldr4 = foldr (fn x => fn v => x^v) "_num" ["1","2","3","4"] = List.foldr (fn (x,v) => x^v) "_num" ["1","2","3","4"]
val test_foldr5 = foldr (fn x => fn init => init andalso x >= 0) true [1,2,3,4] = List.foldr (fn (x, init) => init andalso x >= 0) true [1,2,3,4] 

val test_merge_sort1 = merge_sort [1,2,5,1,2,5,1] = [1,1,1,2,2,5,5]
val test_merge_sort2 = merge_sort [0,3,7,5,0,8,3] = [0,0,3,3,5,7,8]
val test_merge_sort3 = merge_sort [3,2,6,5,8,9,2] = [2,2,3,5,6,8,9]


(* Problem 8. unit tests *)
val test_partition1 = partition (fn x => x > 0) [3,4,~2,6,~3,5] = ([3,4,6,5],[~2,~3])
val test_partition2 = partition (fn x => x < 0) [3,4,~2,6,~3,5] = ([~2,~3],[3,4,6,5])
val test_partition3 = partition (fn x => x mod 2 = 0) [1,2,3,4,5,6,7,8] = ([2,4,6,8],[1,3,5,7])
val test_partition4 = partition (fn x => x >= 0) [1,~2,3,~4,5,6,7,~8] = ([1,3,5,6,7],[~2,~4,~8])
val test_partition5 = let val charls = partition Char.isUpper (String.explode "slhajJKVHJHhvaln")
                      in (fn (xs,ys) => String.implode (xs@ys)) charls = "JKVHJHslhajhvaln"
					  end 
			  
val test_qsort1 = qsort [8,4,5,2,9]  = [2,4,5,8,9]	
val test_qsort2 = qsort [3,2,9,4,8] = [2,3,4,8,9]
val test_qsort3 = qsort [1,~9,8,~4,4] = [~9,~4,1,4,8] 			  
					  

(* Problem 9. unit tests *)
val repeat = fn m => unfold (fn n => if n = 0 then NONE else SOME(m, n-1)) 
val test_unfold1 = countdown 5 = [5,4,3,2,1]	
val test_unfold2 = countdown 3 = [3,2,1]
val test_unfold3 = countdown 6 = [6,5,4,3,2,1]				  
val test_unfold4 = repeat 3 4 = [3,3,3,3] 
val test_unfold5 = repeat #"a" 4 = [#"a",#"a",#"a",#"a"]
val test_unfold6 = repeat ("abc",[3]) 2 = [("abc",[3]),("abc",[3])]  
val test_unfold7 = unfold (fn x => if x > 3 then NONE else SOME (x,x + 1)) 0 = [0, 1, 2, 3]
val test_unfold8 = unfold (fn _ => NONE) false = []
val test_unfold9 = unfold (fn str => if String.size str > 12 then NONE else SOME (str, "Banana" ^ str)) "" = ["", "Banana", "BananaBanana"]


(* Problem 10. unit tests *)
val test_fact2 = let val testls = map (fn n => fold_factorial n = fact' n) [0,1,2,3,4,5,6,7,8,9,10,11,12]
                 in foldl (fn (x,acc) => acc andalso x) true testls 
				 end                 


(* Problem 11. unit tests *)
val test_fold_map1 = fold_map (fn x => 2*x) [1,2,3,4] = [2,4,6,8]
val test_fold_map2 = fold_map (fn x => x + 1) [1,2,3,4] = [2,3,4,5]
val test_fold_map3 = fold_map (fn x => String.size x) ["a","ab","abc"] = [1,2,3]
val test_fold_map4 = fold_map (fn x => x*x) [1,2,3,4] = [1,4,9,16] 
val test_fold_map5 = fold_map (fn x => x) [] = []
val test_fold_map6 = fold_map not [true, false, true, false] = [false, true, false, true]
val test_fold_map7 = fold_map (fn x => "fnord " ^ x) ["a", "quick", "brown", "fox"] 
                   = ["fnord a", "fnord quick", "fnord brown", "fnord fox"]


(* Problem 12. unit tests *)
val test_fold_filter1 = fold_filter (fn x => x mod 2 = 0) [1,2,3,4] = [2,4]
val test_fold_filter2 = fold_filter (fn x => x mod 3 = 0) [1,2,3,4] = [3]
val test_fold_filter3 = fold_filter (fn x => length x = 2) [[1,2],[1,2,3],[]] = [[1,2]]
val test_fold_filter4 = fold_filter Char.isUpper [#"A",#"b",#"C",#"d"] = [#"A",#"C"]	
val test_fold_filter5 = fold_filter (fn x => x) [] = []
val test_fold_filter6 = fold_filter not [true, false, true, false] = [false, false]
val test_fold_filter7 = fold_filter (fn x => String.size x < 4) ["a", "quick", "brown", "fox"] = ["a", "fox"]


(* Problem 13. unit tests *)	
val test_myFoldl1 = myFoldl (fn (x, acc) => x + acc) 0 [1,2,3,4] = foldl (fn (x, acc) => x + acc) 0 [1,2,3,4] 
val test_myFoldl2 = myFoldl (fn (x,v) => x * v) 1 [1,2,3,4] = foldl (fn (x,v) => x * v) 1 [1,2,3,4] 
val test_myFoldl3 = myFoldl (fn (x,v) => (String.size x) + v) 0 ["1","2","3","4"] = foldl (fn (x,v) => (String.size x) + v) 0 ["1","2","3","4"] 
val test_myFoldl4 = myFoldl (fn (x,v) => x^v) "_num" ["1","2","3","4"] = foldl (fn (x,v) => x^v) "_num" ["1","2","3","4"]
val test_myFoldl5 = myFoldl (fn (x, init) => init andalso x >= 0) true [1,2,3,4] = foldl (fn (x, init) => init andalso x >= 0) true [1,2,3,4] 


(* Problem 14. unit tests *)	
val t1 = Node (10, Node (5, Leaf, Leaf), Node (3, Leaf, Leaf))
val t2 = Node (6, Node (3, Node (1,Leaf,Leaf), Node (1,Leaf,Leaf)), Node (2, Node (1,Leaf,Leaf), Leaf))
val t3 = Node ("a",Leaf,Node ("ab", Node ("abc",Leaf,Leaf),Leaf))
                   
val test_tree_map2 = tree_map (fn x => x*x) t1 
                   = Node (100, Node (25, Leaf, Leaf), Node (9, Leaf, Leaf)) 
val test_tree_map3 = tree_map (fn x => x+1) t2 
	               =  (Node (7, Node (4, Node (2,Leaf,Leaf), Node (2,Leaf,Leaf)), Node (3, Node (2,Leaf,Leaf), Leaf)))
val test_tree_map4 = tree_map String.size (Node ("a",Leaf,Node ("ab", Node ("abc",Leaf,Leaf),Leaf)))
                   = (Node (1,Leaf,Node (2, Node (3,Leaf,Leaf),Leaf)))				   					
					
val test_tree_fold1 = tree_fold (fn x => fn l => fn r => x + l + r) 0 t1 = 18
val test_tree_fold2 = tree_fold (fn x => fn l => fn r => x * l * r) 1 t2 = 36 				
val test_tree_fold3 = tree_fold (fn x => fn l => fn r => l ^ x ^ r) "!" (Node ("foo", Node ("bar",Leaf,Leaf), Node ("baz",Leaf,Leaf))) = "!bar!foo!baz!" 
val test_tree_fold4 = tree_fold (fn x => fn l => fn r => x - l - r) 1 (Node (10, Node (5,Leaf,Leaf), Node (3,Leaf,Leaf))) = 6 

val test_tree_filter1 = tree_filter (fn x => x mod 2 = 0) (Node (10, Node (5,Leaf,Leaf), Node (4,Leaf,Leaf))) 
                      = (Node (10,Leaf, Node (4,Leaf,Leaf))) 
val test_tree_filter2 = tree_filter (fn x => String.size x = 1) (Node ("a",Leaf,Node ("ab", Node ("abc",Leaf,Leaf),Leaf)))
                      = (Node ("a",Leaf,Leaf))		
val test_tree_filter3 = tree_filter (fn x => x = 10) t1 = (Node (10,Leaf,Leaf)) 	
val test_tree_filter4 = tree_filter (fn x => x = 5) t1 = Leaf 	
