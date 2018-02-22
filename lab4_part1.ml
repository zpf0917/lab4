(*
                              CS51 Lab 4
                          Modules & Functors

Objective:

This lab practices concepts of modules, including files as modules,
signatures, polymorphic abstract types, and functors.

There are 5 total parts to this lab. Please refer to the following
files to complete all exercises:

-> lab4_part1.ml -- Part 1: Implementing modules (this file)
   lab4_part2.ml -- Part 2: Files as modules
   lab4_part3.ml -- Part 3: Interfaces as abstraction barriers
   lab4_part4.ml -- Part 4: Polymorphic abstract types
   lab4_part5.ml -- Part 5: Functors

 *)

(*======================================================================
Part 1: Implementing Modules

Modules are a way to encapsulate values and functions into separate
components. We may use a module to group related functions together,
for example.

It's also frequently useful to apply signatures to modules. The
signature guarantees that the module implements at least the values
and functions defined within it. The module may also implement more
as well, for internal use, but only those specified in the signature
will be available outside the module definition.

Below is a MATH signature; we'll use it to describe a limited subset of
functions and values that a math module might contain.
......................................................................*)

module type MATH =
  sig
    val pi : float
    val cos : float -> float
    val sin : float -> float
    val sum : float -> float -> float
    val max : float list -> float option
  end ;;

(*......................................................................
Exercise 1A: Complete the implementation of a module called Math that
satisfies the signature above. (The value `nan` stands for "not a
number" and is an actual value of the float type. We're using it here
as a temporary value pending your putting in appropriate ones.)
......................................................................*)

module Math : MATH =
  struct
    let pi = acos (-1.0)
    let cos = cos
    let sin = sin
    let sum = (+.)
    let max a = Some (List.fold_left max min_float a)
  end ;;

(*......................................................................
Exercise 1B: Now that you've implemented the Math module, use it to
compute the maximum of the cosine of pi and the sine of pi, a value of
type float option. Name the resulting value `result`. (Do not use
the `open` command for this exercise.)
......................................................................*)

let result = Math.max [(Math.cos Math.pi); (Math.sin Math.pi)] ;;

(*......................................................................
Exercise 1C: Redo the computation from above, but use the `local open`
syntax to write your computation in a more succinct manner.
......................................................................*)

let result_local_open =
  let open Math in
    max[cos pi; sin pi];;
