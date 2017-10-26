(* Assignment 1 Supplementation
Hersi, Ibrahim Abdirahman 	
Kisha, Hamid
*)
 (* problem 1*)
(* 1 evaluation steps of product 3
product 3;
3 * product (3 - 1);
3 * product (2);
3 * 2 * product (2 - 1);
3 * 2 * product (1);
3 * 2 * 1;
6 
 *)

(* 2 The function “product n” computes the factorial of n which is the product of numbers from 1 up to n inclusive *)
(* 3 Specification
   fun product n
   TYPE: int -> int
   PRE: n  ≥ 1
   POST: n * (n-1) * (n-2) * … * 1
*)
(* 4 The variant for the function is n *)


(* problem 2*)
(* 1 *)
val minus = fn x => fn y => x - y;

(* 2 *)
val foo = 1: int;

(* 3 
val foo = fn: int -> int;
 *)

(* 4
minus 5 4;
5 – 4;
1;
*)
(* problem 3 *)
fun fun1 x = x + 1;
fun fun2 x y = x + y;
fun fun3 x = (x + 1, x - 1);
fun fun4 (x,y) = x + y;
fun fun5 x y z = if x < ceil(y) then Int.toString(x * ceil(y)) ^ z else Int.toString(x * floor(y)) ^ z;
fun fun6 (x , (a, b, y)) = if x > y then (x - y, a ^ b) else ( y - x, b ^ a);

(* fun gcd (m,n)
   TYPE: int * int -> int
   PRE: m,n  ≥ 0 and m + n > 0
   POST: the greatest common divisor of m and n
*)	
fun gcd(0 , n) = n
  | gcd(m , n) = gcd(n mod m , m);

(* fun lcm_helper n result
   TYPE: int -> int -> int
   POST: the smallest positive number that is evenly divisible by all of the numbers from 1 to n
   VARIANT: n
*)
fun lcm_helper 1 result = result
  | lcm_helper n result = lcm_helper (n - 1) (result * (op div)(n , gcd(result , n)));

(* fun lcm n
   TYPE: int -> int
   PRE: n does not equal to 0
   POST: the smallest positive number that is evenly divisible by all of the numbers from 1 to n.
*)
fun lcm n =
	if n < 1 then lcm_helper (0 - n) (0 - n) else lcm_helper n n;

