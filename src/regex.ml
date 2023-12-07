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
    match e with
    |Eps -> true
    |Base a -> true
    |Joker -> true
    |Concat(a,b) -> is_finite a && is_finite b
    |Alt(a,b) -> is_finite a && is_finite b
    |Star a -> is_empty a

  let product l1 l2 =
    List.fold_left
      (fun acc x -> List.fold_left (fun acc y -> (y @ x) :: acc) acc l1)
      [] l2

let enumerate alphabet e =
  failwith "À compléter"

let rec alphabet_expr e =
  failwith "À compléter"

type answer =
  Infinite | Accept | Reject

let accept_partial e w =
  failwith "À compléter"
