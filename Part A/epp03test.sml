(* Author  : Pieter van Wyk
 * Created : 2020-04-21
 * Updated : 2020-04-28
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
val devideOutN = fn n => do_until (fn x => x div n) (fn x => x mod n = 0)
val devideOutTwo   = devideOutN 2 
val devideOutThree = devideOutN 3 

val test_do_until1 = devideOutTwo 8 = 1
val test_do_until2 = devideOutTwo 20 = 5 
val test_do_until3 = devideOutThree 9 = 1 
val test_do_until4 = devideOutN 9 27 = 3 


(* Problem 3. unit tests *)
val test_fact = let val testls = map (fn n => fact n = fact' n) [1,2,3,4,5,6,7,8,9,10,11,12]
                in foldl (fn (x,acc) => acc andalso x) true testls 
				end 
				

(* Problem 4. unit tests *)	
val test_fixed_point1 = fixed_point (fn n => n div 5) 25 = 0
val test_fixed_point2 = fixed_point (fn n => n div 3) 64 = 0
val test_fixed_point3 = fixed_point (fn n => n div 7) 17 = 0
val test_fixed_point4 = fixed_point (fn n => n div 8) 98 = 0   	
		

(* Problem 5. unit tests *)
val test_map2_1 = map2 String.size [("a","ab"),("ab","abc"),("abc","abcd")] = [(1,2),(2,3),(3,4)] 
val test_map2_2 = map2 hd [([1],[2,3]),([4,5],[6])] = [(1,2),(4,6)]
val test_map2_3 = map2 tl [([1],[2,3]),([4,5],[6])] = [([],[3]),([5],[])]
val test_map2_4 = map2 (fn n => n mod 2 = 0) [(1,2),(2,3),(3,4)] = [(false,true),(true,false),(false,true)]


(* Problem 5. unit tests *)
val test_app_all1 =
    let fun f n = [n, 2 * n, 3 * n]
    in app_all f f 1 = [1, 2, 3, 2, 4, 6, 3, 6, 9]
    end 
val test_app_all2 =
    let fun f s = [s^"_1", s^"_2"]
    in app_all f f "test" = ["test_1_1", "test_1_2", "test_2_1", "test_2_2"]
    end 
val test_app_all3 =
    let fun g s = [s, s^s]
	fun f s = [String.size s,3]
    in app_all f g "test" = [4,3,8,3]
    end
val test_app_all4 =
    let fun g n = [devideOutTwo n, devideOutThree n]
	fun f n = [3*n,2*n]
    in app_all f g 24 = [9,6,24,16]
    end		
