(*hamid kisha
ibrahim hersi
 *)

(*Problem 1 and 2*)

(* fun f y sum count
   TYPE: real list * real * real -> real
   PRE: the value of each real curried argument is 0.0
   POST: the average of the elements in y given by sum / count
   VARIANT: length of y
 *)
fun f([] , sum , count) = sum / count
  | f(x::xs, sum, count) = f(xs, x + sum, count + 1.0);

 (* fun average y
   TYPE: real list -> real
   PRE: true
   POST: the average of the elements in ys given by sum / count
   VARIANT: length of ys
 *)
fun average y = 
   case y of
           [] => 0.0
         | _  => f(y,0.0,0.0); 

(*Problem 3*)

(*fun append a b
  TYPE   : 'a list -> 'a list -> 'a list
  PRE    : true
  POST   : the list that contains the elements in a and the elements in b  
  VARIANT: length of a plus length of b
*)
fun append a b = foldr (op ::) b a;

(*fun member a y
  TYPE   : ''a -> ''a list -> bool
  PRE    : true
  POST   : true if a is member in the list y  
  VARIANT: length of y
*)
fun member a y = foldl (fn(x,z) => z orelse x = a) false y;

(*fun last y
  TYPE   : 'a list -> 'a
  PRE    : true
  POST   : the last element in the list y  
  VARIANT: length of y
*)
fun last ([]) = raise Domain
  | last (y::ys)  = foldl (fn(x,_) => x) y ys;

(*fun reverse y
  TYPE   : 'a list -> 'a list
  PRE    : true
  POST   : the reverse of the list y  
  VARIANT: length of y
*)
fun reverse y = foldl (op ::) [] y;

(*fun filter f y
  TYPE   : ('a -> bool) -> 'a list -> 'a list
  PRE    : true
  POST   : the list of elements of y that return true when applied to f   
  VARIANT: length of y
*)
fun filter test a = foldr (fn(x,ys) => if test(x) then x::ys else ys) [] a;

(*Problem 4*)

datatype tree = Void | Node of tree * int * tree;

(*fun insert t1 t2
  TYPE   : tree * tree -> tree
  PRE    : true
  POST   : the tree that result from inserting the tree t1 into the tree t2   
  VARIANT: depth of t2 tree
*)
fun insert(t1 , Void) = t1
  | insert (Void, t2) = t2			    
  | insert(Node(a, x, b), Node(m, y, n)) = if x > y then Node(m , y, insert(Node(a, x, b), n))
                                           else Node(insert(Node(a, x, b), m), y, n);

(*fun sub_tree a b t
  TYPE   : int -> int -> tree -> tree
  PRE    : true
  POST   : a tree containing the keys in t that are greater than or equal to a but smaller than b.   
  VARIANT: depth of tree t
*)
fun sub_tree a b t =
    let
       fun f(Void) = Void
         | f(Node(lt, x, rt)) = if a <= x andalso x < b then insert(f(lt), insert(f(rt), Node(Void, x, Void)))
                                else insert(f(lt), f(rt))
    in
      f(t)
    end;

(*
  The function builds the required tree in two steps:
     1. search for nodes that satisfy the test.
     2. build the tree by adding each node to the previously found ones using the helper function "insert".
  The complexity of the algorithm is the sum of the complexity of step 1 and step 2.
     1. The complexity of step 1 is n*C1 (i.e the number of nodes in the tree multiplied by a constant) because the function searches all nodes
        to find whether they are satisfying the test or not.
     2. The complexity of step 2:
        1- we insert the first found node into an empty tree, the cost here is constant, say 0.
        2- When we find the next node(node2), the insert function compares the value of node2 label with that of node1,cost here is C2
           we compare with node1 only, suppose its greater than node1, so it is put at the right of node1.
        3- When we add node3, at the worst case it is larger than both of node1 and node2. Thus, we compare node3 with node1 first then with node2.
           The cost here is double the cost in 2, so it is 2*C2.
        n- When we arrive at node n( this is the worst case, i.e all nodes satisfy the condition and every time the new node is put to the right of the last added node),
           the cost here is (n-1)*C2.
        Therfore the complexity of inserting the nodes is:
           C2 + 2*C2 + 3*C2 + ... + (n-1)*C2 = (n)*(n-1)(C2)/2 
        The complexity of the algorithms is : n*C1 +  (n)*(n-1)(C2)/2 = O(n^2)
        So it is quadratic.
*)
