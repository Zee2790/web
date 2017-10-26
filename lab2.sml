(*Hamid Kisha
  Ibrahim Hersi*)

(*Problem 1*)

(* iota n
     TYPE: int -> int list
     PRE: n >= 0
     POST: the list [0, 1, 2, ..., n-1]
     EXAMPLES: iota 3 = [0, 1, 2]
 *)
fun iota n =
  let
    (* iota' m
       TYPE: int -> int list
       PRE: m >= 0
       POST: the list [0, 1, 2, ..., n-1]
       VARIANT: m 
     *)
    fun iota' 0 = []
      | iota' m = (n-m) :: iota' (m-1)
  in
    if n < 0 then
      raise Domain
    else
      iota' n
  end;

(*Problem 2*)

(* ismember(x,y)
     TYPE    : ''a * ''a list -> bool
     PRE     : true
     POST    : is x in the list y
     VARIANT : length of y
     EXAMPLES: ismember(3, [0, 1]) = false
 *)
fun ismember(x, []) = false
  | ismember(x, y::ys) = if x = y then 
				      true 
				    else 
					 ismember(x, ys);


(* inter s1 s2
     TYPE    : ''a list -> ''a list -> ''a list	
     PRE     : true
     POST    : a list of elements that are in both of s1 and s2
     VARIANT : length of s1
     EXAMPLES: inter [3, 1] [0, 3, 5] = [3]
 *)
fun inter s1 s2 =
   let
       val pair = (s1,s2)
   in
       case pair of
 	     ([],_) => []
       |  (x::xs, ys) => if ismember(x,ys) then 
					   x :: inter xs ys 
				    else 
					   inter xs ys
   end;		
				  
(* inter' s1 s2
     TYPE    : int list -> int list -> int list	
     PRE     : the elements in the lists are in an increasing order
     POST    : a list of elements that are in both of x and y
     VARIANT : legth of s1 plus length of s2
     EXAMPLES: inter [1, 3] [0, 3, 5] = [3]
 *)
fun inter' s1 s2 =
    let
 	val pair = (s1,s2)
    in
 	case pair of
 		    ([],_) => []
 		  | (_,[]) => []
 		  | (x::xs, y::ys) => if x = y then x :: inter' xs ys
 				           else if x < y then inter' xs s2
 				           else inter' s1 ys
    end;

(*
result;
val it =
   ((12.965, [0, 1, 2, 3, 4, 5, 6, 7, ...]),
    (0.085, [0, 1, 2, 3, 4, 5, 6, 7, ...])):
   (Time.time * int list) * (Time.time * int list)

the slow time is 12.965
the fast time is 0.085
*)

(*Problem 3*)

datatype fruit = Apple of real
		    | Banana of real
 		    | Lemon of int;


(* sumPrice fruits a b c
     TYPE: fruit list -> real -> real -> real -> real	
     PRE: true
     POST: total price of all fruits in the given list
	Variant: the length of fruit list
     EXAMPLES: sumPrice [Apple 3.0, Lemon 1] 2.5 2.0 1.0 
			= 3.0 * 2.5 + 1.0 * 1.0 = 8.5
 *)
fun sumPrice fruits a b c =
   let
      fun sumPrice' ([]) = 0.0
 	| sumPrice' ( (Apple x)::ys) = x * a + sumPrice' ys
 	| sumPrice' ( (Banana x)::ys) = x * b + sumPrice' ys
 	| sumPrice' ( (Lemon x)::ys) = real(x) * c + sumPrice' ys
   in
      sumPrice' fruits
   end;
		
(*Problem 4*)

datatype 'a ltree =  Node of 'a * 'a ltree list;

(* count tree
   TYPE   : 'a ltree -> int
   PRE    : true
   POST   : the number of nodes in tree
   VARIANT: the number of children of tree 
*)
fun count(Node(a,[])) = 1
  | count(Node(a,x::xs)) = count(x) + count(Node(a,xs));

(* labels tree
   TYPE   : 'a ltree -> 'a list
   PRE    : true
   POST   : the list of all node labels in tree
   VARIANT: the number of children of tree
*)
fun labels tree =
  (* labels' tree tree_list labels_list
     TYPE   : 'a ltree * 'a ltree list * 'a list -> 'a list
     PRE    : true
     POST   : the list of all node labels in tree
     VARIANT: the number of children of tree 
  *)  
  let fun labels' (Node(a,[]) , [] , z) = a::z
	| labels' (Node(a,[]) , y::ys , z) = labels' (y , ys , a::z) 
	| labels' (Node(a , x::xs), y , z) = labels' (Node(a , xs) , x::y, z)  
   in
     labels' (tree , [], [])
  end;

(* is_present tree value
   TYPE   : ''a ltree -> ''a -> bool
   PRE    : true
   POST   : true if a node in tree is labeled with value
   VARIANT: the number of children of tree 
 *)
fun is_present tree value = 
 (* is_present' tree 
    TYPE   : ''a ltree -> bool
    PRE    : true
    POST   : true if a node in tree is labeled with value
    VARIANT: the number of children of tree 
  *)
  let fun ispresent' ([]) = false
       | ispresent' (x::xs) = if x = value then true else ispresent' (xs)
  in
   ispresent' (labels tree)
  end;

(* height tree 
   TYPE   : 'a ltree -> int
   PRE    : true
   POST   : the height of tree
   VARIANT: the number of children of tree 
*)
fun height (Node(a , [])) = 1
  | height (Node(a , x::xs)) = Int.max(1 + height(x) , height(Node(a, xs))) 

