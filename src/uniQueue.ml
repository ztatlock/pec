(* uniQueue.ml : FIFO Queues With No Repeats *)

(* Note: In the funcs below, we make the queue the first arg.
 *       This simplifies using "push" with List.fold_left. 
 *)

exception Empty

type 'a t =
  Q of 'a list * 'a list

(* internal *)

let norm = function
  | Q ([], b) -> Q (List.rev b, [])
  | q -> q

let add q e =
  match q with
  | Q (f, b) -> norm (Q (f, e::b))

(* external *)

let empty =
  Q ([], [])

let is_empty q = 
  q = empty
  
let mem q e =
  match q with
  | Q (f, b) ->
      List.mem e f ||
      List.mem e b

(* only add new elts *)
let push q e =
  if mem q e then
    q
  else
    add q e

(* return pair of first elt and rest of queue *)
let pop = function
  | Q (f::fs, bs) -> (f, norm (Q (fs, bs)))
  | _             -> raise Empty

