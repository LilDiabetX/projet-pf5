open Regex_base

let rec repeat n l =
  failwith "À compléter"

let rec expr_repeat n e =
  failwith "À compléter"

  let rec is_empty e =
    match e with
    |Eps -> true
    |Base a -> false
    |Joker -> false
    |Concat(a,b) -> is_empty a && is_empty b
    |Alt(a,b) -> is_empty a && is_empty b
    |Star a -> is_empty a 

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
