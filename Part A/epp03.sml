(* Author  : Pieter van Wyk
 * Created : 2020-04-21
 * Updated : 2020-05-01
 *
 * Solutions to the extra practice problems of week 4 of part A 
 *)

(* Problem 1.
 * Write a function compose_opt : ('b -> 'c option) -> ('a -> 'b option) -> 'a -> 'c option
 * that composes two functions with "optional" values. If either function returns NONE, then the result is NONE.
 *)

fun compose_opt f_opt g_opt = fn x =>
    case (g_opt x) 
    of NONE => NONE 
     | SOME x' => f_opt x'
	
	
(* Problem 2.
 * Write a function do_until : ('a -> 'a) -> ('a -> bool) -> 'a -> 'a. do_until f p x will apply f to x
 * and f again to that result and so on until p x is false. Example: do_until (fn x => x div 2) (fn x => x mod 2 <> 1)
 * will evaluate to a function of type int->int that divides its argument by 2 until it reaches an odd number. 
 * In effect, it will remove all factors of 2 its argument.
 *)

fun do_until f p = fn x => 
    case (p x) 
    of true  => do_until f p (f x)
     | false => x  
	

(* Problem 3.
 * Use do_until to implement factorial.
 *)

fun fact n =
    if n = 0 then 1
    else let val (ans,_) = do_until (fn (acc,m) => (acc*m,m-1)) (fn (_,m) => m <> 1) (1,n)
	 in ans 
	 end 

fun fact' n = (* for testing *)
    if n = 0 then 1
    else n * fact' (n - 1)

	
(* Problem 4. 
 * Use do_until to write a function fixed_point: (''a -> ''a) -> ''a -> ''a that given a function f 
 * and an initial value x applies f to x until f x = x. (Notice the use of '' to indicate equality types.)
 *)	
 
fun fixed_point f = do_until f (fn x => (f x <> x))   
		 
 
(* Problem 5.
 * Write a function map2 : ('a -> 'b) -> 'a * 'a -> 'b * 'b that given a function that takes ’a values to ’b
 * values and a pair of ’a values returns the corresponding pair of ’b values.
 *)
 
fun map2 f (x,y) = (f x,f y)
  
  
(* Problem 6.
 * Write a function app_all : ('b -> 'c list) -> ('a -> 'b list) -> 'a -> 'c list, so that: 
 * app_all f g x will apply f to every element of the list g x and concatenate the results 
 * into a single list. For example, for fun f n = [n, 2 * n, 3 * n], we have 
 * app_all f f 1 = [1, 2, 3, 2, 4, 6, 3, 6, 9].
 *)
 
fun concat xss = foldl (fn (xs,vs) => vs@xs) [] xss
fun app_all f g x = concat (map f (g x))
 	
  
(* Problem 7.
 * Implement List.foldr
 * f init [x1, x2, ..., xn] returns f(x1, f(x2, ..., f(xn, init)...))
 * or init if the list is empty.
 *)
 
fun foldr _ v [] = v 
  | foldr f v (x::xs) = f x (foldr f v xs)  
  
(* extra practice : use foldr to define merge sort *)
fun merge xs [] = xs
  | merge [] ys = ys
  | merge (x::xs) (y::ys) =
    case (x < y) 
    of true  => x::(merge xs (y::ys))
     | false => y::(merge (x::xs) ys)	

fun break n xs = 
    let fun break' x (accL,accR,m) =
	    case (m = 0) 
	    of true  => (x::accL,accR,0)
	     | false => (accL,x::accR,m-1) 
	val (lsL,lsR,_) = foldr break' ([],[],n) xs 
    in (lsL,lsR)
    end 	
	        
fun merge_sort []  = []
  | merge_sort [x] = [x]
  | merge_sort xs = 
    let val (lsL,lsR) = break ((length xs) div 2) xs
    in merge (merge_sort lsL) (merge_sort lsR) 
    end    	
	

(* Problem 8.
 * Write a function partition : ('a -> bool) -> 'a list -> 'a list * 'a list 
 * where the first part of the result contains the second argument elements for 
 * which the first element evaluates to true and the second part of the result 
 * contains the other second argument elements. Traverse the second argument only once.
 *)	
 
fun partition p xs =
    let fun partition' x (accT,accF) =
	    case (p x) 
	    of true  => (x::accT, accF) 
             | false => (accT, x::accF)  						   
    in foldr partition' ([],[]) xs 
    end	
	
	
(* extra practice : use partition to define quick sort *)
fun qsort []  = []
  | qsort [x] = [x] 
  | qsort (x::xs) =
    let val (small_ls,large_ls) = partition (fn el => el <= x) xs 
    in concat [(qsort small_ls),[x],(qsort large_ls)]
    end 	

	
(* Problem 9.
 * Write a function unfold : ('a -> ('b * 'a) option) -> 'a -> 'b list that produces a list of 
 * ’b values given a "seed" of type ’a and a function that given a seed produces SOME 
 * of a pair of a ’b value and a new seed, or NONE if it is done seeding. For example, here is 
 * an elaborate way to count down from 5: 
 * unfold (fn n => if n = 0 then NONE else SOME(n, n-1)) 5 = [5, 4, 3, 2, 1]
 *)	
 
fun unfold f n =
    case (f n)
    of NONE => [] 
     | SOME (m,m') => m::(unfold f m')
	 
	 
(* Problem 10.
 * Use unfold and foldl to implement factorial.
 *)
 
val countdown = unfold (fn n => if n = 0 then NONE else SOME(n, n-1))	
fun fold_factorial n = foldl (fn (x,v) => v*x) 1 (countdown n) 
 
  
(* Problem 11. 
 * Implement map using List.foldr.
 *)
 
fun fold_map f xs = List.foldr (fn (x,v) => (f x)::v) [] xs 
 
 
(* Problem 12. 
 * Implement filter using List.foldr.
 *)  

fun fold_filter p xs = List.foldr (fn (x,v) => if (p x) then x::v else v) [] xs
 
 
(* Problem 13. 
 * Implement foldl using foldr on functions. 
 *) 
 
fun myFoldl f v xs = 
    let val rev_xs = foldr (fn x => fn v => v@[x]) [] xs
	fun f' x v = f (x,v)
    in foldr f' v rev_xs 
    end
 
 
(* Problem 14.
 * Define a (polymorphic) type for binary trees where data is at internal nodes 
 * but not at leaves. Define map and fold functions over such trees. You can define 
 * filter as well where we interpret a "false" as meaning the entire subtree rooted
 * at the node with data that produced false should be replaced with a leaf.
 *)
datatype 'a tree = Leaf | Node of ('a * 'a tree * 'a tree) 

(* map over tree *)
fun tree_map f tr =
    case tr
    of Leaf => Leaf 
     | Node (x,trL,trR) => Node (f x, tree_map f trL, tree_map f trR) 
	 
(* fold over tree *)
fun tree_fold f acc tr =
    case tr
    of Leaf => acc 
     | Node (x,trL,trR) => f x (tree_fold f acc trL) (tree_fold f acc trR)	
	 
(* filter over tree *)	 
fun tree_filter p tr = 
    case tr 
    of Leaf => Leaf 
     | Node (x,trL,trR) => if (p x) 
	                   then Node (x, tree_filter p trL, tree_filter p trR)
                           else Leaf						   
	 
	 
(* END *)
