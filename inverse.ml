(*
     A collection of statements contained in an "Inverse" module.
 *)

type length = Infinite | Finite of int ;;

(* info -- A type containing some random information about inverses *) 
type info = {
   name : string;
   inventor : string option;
   length : length
}

let name = "f(x) = 1 / x" ;;
let length = Infinite ;;
let inventor = None ;;

let info = {
   name;
   length;
   inventor;
} ;;

(* eval -- Return the multiplicative inverse of n if defined *)
let eval n = 
  if n = 0
  then None
  else Some (1. /. float_of_int n) ;;

(* exists -- Preeicate checks if n has a multiplicative inverse *)
let exists n = 
  n <> 0 ;;
