(* Group assignment 1G in IP2013
 * Authors      : Paw Saabye Pedersen <paw @ pawhome.net>
 *              : Halfdan Keller Justesen
 * To be submitted thursday *)

(* Library Imports {{{ *)
open Math;
(*}}}*)

(* Assignment 1G1 : Solving equations {{{ *)

(* solve2 : real * real * real -> real * real
 * Returns the solution(s) for the 2nd degree equation denoted by
 * ax^2 + bx + c
 * Expects: a,b,c as reals *)
fun solve2(a,b,c) =
  let
    val disc = pow(b,2.0) - (4.0 * a * c)
    exception Unsolvable;
  in
    if disc < 0.0 orelse a = 0.0
    then raise Unsolvable
    else let
      val n = (2.0 * a)
      val disc' = sqrt(disc)
    in
        ((~b + disc') / n
        ,(~b - disc') / n)
    end
  end;

(* validity test *)
; val _ = solve2 : real * real * real -> real * real;

(*}}}*)
(* Assignment 1G2 : Call test 1 {{{ *)
(* test_solve_01 : bool
 * Tests if calling solve2 with the argument (2.0,3.0,1.0) yields
 * the desired values (-1.0 and -0.5)*)
val test_solve2_01 = solve2(2.0,3.0,1.0) = (~0.5,~1.0);
(*}}}*)
(* Assignment 1G3 : Call test 2 {{{ *)
(* test_solve2_02 : boolean *)
val test_solve2_02 = (solve2(2.0,3.0,4.0); false)
                       handle Unsolvable => true;
(* As the discriminant is less than zero the function throws an exception. *)
(*}}}*)
(* Assignment 1G4 : Power function {{{ *)

(* powerNew : int * int -> int
 * Optimised power function
 * Expects: n >= 0 or else n = |n| *)
fun powerNew(a,0) = 1
  | powerNew(a,n) =
  let
    val tmp = powerNew(a, abs(n) div 2)
    val tmp2 = tmp * tmp
  in
    if n mod 2 = 0 then
      tmp2
    else
      tmp2 * a
  end;

(* validity test *)
; val _ = powerNew : int * int -> int;
;
(*}}}*)
(* Assignment 1G5 : Power with counter {{{ *)
(* powerCount : int * int -> int * int
 * Given two ints (a,n) the function returns (a^n, <no of multiplications used)
 * Expects      : n *)
(* OPGAVE 5 *)
local
  (* multiplications (n,i) : int *int -> int
   * Returns no of multiplications needed to perform the power operation with
   * powerNew.
   * Expects    : a > 0
   *              i = 0 *)
  fun multiplications (0,i) = i
    | multiplications (n,i) =
      if   n mod 2 = 0
      then multiplications(n div 2,i+1)
      else multiplications(n div 2,i+2)
in
  (* powerCount(a,n) : int * int -> int * int 
   * Beregner a^n og antallet af multiplikationer udført til beregningen *)
  fun powerCount(a,n) = (powerNew(a,n),multiplications(n,0))
end;
(*}}}*)

