(*
    A collection of statements contained in a "Studyplan" module.
 *)

(* Possible lengths for study plans, including infinite ones *)
type length =
  | Infinite
  | Finite of int ;;

(* info -- A type containing some random information about this study plan *) 
type info = {
  name : string;      (* the name of the study plan *)
  length : length;    (* the index of the last course in the study plan *)
  inventor : string   (* who invented the study plan *)
} ;;

(* A particular study plan *)
let length = Finite 5 ;;
let name = "CS Concentration Study Plan" ;;
let courses = [| 50; 51; 121; 124; 152; 171 |] ;;

let rec info =
  { name;
    length;
    inventor }
and inventor = "You!" ;;

(* exists -- Predicate holds if the index n is appropriate for the
   `courses` *)
let exists n = 
  n > 0 && n < Array.length courses ;;

(* eval -- Return the n-th course in the study plan *)
let eval n =
  if exists n
  then Some (Array.get courses n)
  else None ;;
