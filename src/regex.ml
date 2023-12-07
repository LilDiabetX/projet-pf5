open Regex_base

let rec repeat n l =
  failwith "À compléter"

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
  let alphabet = List.map (fun x -> [ x ]) alphabet in
  let rec aux = function
    | Eps | Star _ -> [ [] ]
    | Base x -> [ [ x ] ]
    | Joker -> alphabet
    | Concat (e, e') -> product (aux e) (aux e')
    | Alt (e, e') -> aux e @ aux e'
  in
  if is_finite e then Some (aux e) else None

let rec alphabet_expr e =
  failwith "À compléter"

type answer =
  Infinite | Accept | Reject

let accept_partial e w =
  failwith "À compléter"
