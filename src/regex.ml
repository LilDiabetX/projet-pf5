open Regex_base

let repeat n l =
  let rec aux n li acc =
    if n = 0 
      then acc
    else 
      match li with
      |[] -> aux (n - 1) l acc
      |h::t -> aux n t (h::acc)
  in List.rev_append (aux n l []) []


  let rec expr_repeat n e = 
    if n =0
      then Eps
      else
    if n = 1
    then e
    else Concat ( e,(expr_repeat (n-1) e))

let rec is_empty e =
  failwith "À compléter"

let rec null e =
  failwith "À compléter"

let rec is_finite e =
  failwith "À compléter"

let product l1 l2 =
  failwith "À compléter"

let enumerate alphabet e =
  failwith "À compléter"

let rec alphabet_expr e =
  failwith "À compléter"

type answer =
  Infinite | Accept | Reject

let accept_partial e w =
  failwith "À compléter"
