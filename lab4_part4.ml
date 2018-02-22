(*
                              CS51 Lab 4
                          Modules & Functors

Objective:

This lab practices concepts of modules, including files as modules,
signatures, polymorphic abstract types, and functors.

There are 5 total parts to this lab. Please refer to the following
files to complete all exercises:

   lab4_part1.ml -- Part 1: Implementing modules
   lab4_part2.ml -- Part 2: Files as modules
   lab4_part3.ml -- Part 3: Interfaces as abstraction barriers
-> lab4_part4.ml -- Part 4: Polymorphic abstract types (this file)
   lab4_part5.ml -- Part 5: Functors

 *)

(*======================================================================
Part 4: Polymorphic abstract types

You may have noticed that the stack module in Part 3 focused
exclusively on int types. But through signatures, this doesn't have to
be so: we can also create modules with polymorphic abstract types.

Let's create another data structure, a queue, which (unlike a stack)
is a FIFO (First-In First-Out) structure.

As the name implies, an element that is input first through an enqueue
function is the first element to be removed by a dequeue function.
Much like the stack's "pop" function, we will break the "dequeue"
function into two separate operations: a "front" function, that returns
the value of the front element, and a "dequeue" function that returns a
new queue with the front element removed.

Below is a signature for this queue data structure, but it also
includes a polymorphic abstract type so that we can generalize queues
to be int queues, string queues, and so on. After the
signature, we'll immediately dive into implementing the module.
......................................................................*)

module type QUEUE =
  sig
    exception EmptyQueue
    type 'a queue
    val empty : unit -> 'a queue
    val enqueue : 'a -> 'a queue -> 'a queue
    val front : 'a queue -> 'a
    val dequeue : 'a queue -> 'a queue
  end ;;

(*......................................................................
Exercise 4A: Define the queue module. First, decide how you'll
represent the queue, then implement each of the functions based on
your decision.

We helped you out a little and defined "front" and "dequeue" for you,
below. These rely on a helper function called "dequeue_helper", which
is one you must implement. It should accept a queue and return a tuple
containing the first element and the queue with the first element
removed.

Notice that the dequeue_helper function does *not* appear in the
signature, and will therefore not be accessible to functions outside
of the module.

You'll want to take advantage of the EmptyQueue exception provided in
the module; raise it if an attempt is made to examine or dequeue the
front of an empty queue.
......................................................................*)

module Queue : QUEUE =
  struct
    exception EmptyQueue

    type 'a queue = 'a list     (* replace this with the correct
                               implementation type *)

    let empty () : 'a queue =
      []

    let enqueue (el : 'a) (q : 'a queue) =
      q @ [el]

    (* dequeue_helper q -- Returns a pair of the front element of the
       queue and a queue containing the remaining elements *)
    let dequeue_helper (q : 'a queue) : ('a * 'a queue) =
      match q with
      | [] -> raise EmptyQueue
      | hd :: tl -> (hd, tl)

    let front (q: 'a queue) : 'a =
      fst (dequeue_helper q)

    let dequeue (q : 'a queue) : 'a queue =
      snd (dequeue_helper q)
  end ;;

(*......................................................................
Most likely, you implemented queues with a list. If you did, you
probably had to use a list append (rather than cons, as we did with
the stack) operation. As a result, enqueue requires O(n) time. We're
not too concerned with performance right now, but realize that there
is a way to create a queue with constant time enqueue operation that
still is fairly fast in the other functions, too. (We'll be discussing
linear time implementations of queues in lecture 10.)  Since this
module's implementation was abstracted with a signature, you can later
go back and improve it without breaking any code. But again, don't
worry about performance for now; we only care that your implementation
returns the correct results.

........................................................................
Exercise 4B: Write a function, q, that takes a unit argument and uses
your Queue module to return a new queue with the following strings
enqueued in order: "Computer", "Science", "51".
......................................................................*)

let q = fun _ -> failwith "not implemented" ;;

(*......................................................................
Exercise 4C: Write an expression to generate a queue with the q
function, above, and pull the front element from it, naming the result
front_el.
......................................................................*)

let front_el = "replace me with an expression using the Queue module" ;;
