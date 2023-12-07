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
    match e with
    |Eps -> true
    |Base a -> false
    |Joker -> false
    |Concat(a,b) -> is_empty a && is_empty b
    |Alt(a,b) -> is_empty a && is_empty b
    |Star a -> is_empty a 

let rec null e =
  match e with
  |Eps -> true
  |Base a -> false
  |Joker -> false
  |Concat(a,b) -> null a && null b
  |Alt(a,b) -> null a || null b
  |Star a -> true

let rec is_finite e =
  failwith "À compléter"

let product l1 l2 =
  failwith "À compléter"

let enumerate alphabet e =
  failwith "À compléter"

let alphabet_expr e =
  let rec alpha_expr acc = function
    | Eps | Joker -> acc
    | Base x -> x :: acc
    | Star x -> alpha_expr acc x
    | Concat (e, e') | Alt (e, e') -> alpha_expr (alpha_expr acc e) e'
  in
  alpha_expr [] e
  |> List.sort_uniq (fun a b -> if a = b then 0 else if a < b then -1 else 1)

type answer =
  Infinite | Accept | Reject

let accept_partial e w =
  failwith "À compléter"
