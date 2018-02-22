(*
    A collection of statements contained in a "Fibonacci" module.
 *)

type length =
  | Infinite
  | Finite of int ;;

(* info -- A type containing some random information about the
   Fibonacci sequence *)
type info = {
  name : string;      (* evocative name for this module *)
  length : length;    (* the number of years this idea will be important *)
  inventor : string   (* the inventor of the ideas in this module *)
} ;;

let name = "Fibonacci" ;;
let length = Infinite ;;
let inventor = "Leonardo of Pisa" ;;

let info = {
   name;
   length;
   inventor;
} ;;

(* exists -- Predicate holds if n is an appropriate fibonacci index *)
let exists n = 
  n > 0 ;;
	
(* eval -- Return the n-th fibonacci number *)
let eval n = 
  let rec ev n =
    if (n = 1 || n = 2)
    then 1
    else ev (n - 1) + ev (n - 2)
  in
  Some (ev n) ;;

