(***********************************************************************
**  Code retrieved on 2012-06-27 from
**  http://caml.inria.fr/pub/docs/manual-ocaml/manual004.html
**
**  Only minor modifications done. Added the functions is_empty and top.
**
**  Note that the order of elements with equal priority is lost.
************************************************************************)

module PrioQueue =
struct

  type priority = int
  type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue

  exception Queue_is_empty

  let empty = Empty

  let is_empty = function
    | Empty -> true
    |  _ -> false

  let rec insert queue prio elt =
    match queue with
      | Empty -> Node(prio, elt, Empty, Empty)
      | Node(p, e, left, right) ->
        if prio <= p
        then Node(prio, elt, insert right p e, left)
        else Node(p, e, insert right prio elt, left)

  let top = function
    | Empty -> raise Queue_is_empty
    | Node(prio, elt, _, _) -> (prio, elt)
      
  let rec remove_top = function
    | Empty -> raise Queue_is_empty
    | Node(prio, elt, left, Empty) -> left
    | Node(prio, elt, Empty, right) -> right
    | Node(prio, elt, (Node(lprio, lelt, _, _) as left),
           (Node(rprio, relt, _, _) as right)) ->
      if lprio <= rprio
      then Node(lprio, lelt, remove_top left, right)
      else Node(rprio, relt, left, remove_top right)

  let extract = function
    | Empty -> raise Queue_is_empty
    | Node(prio, elt, _, _) as queue -> (prio, elt, remove_top queue)

end;;
