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
  let alphabet = List.map (fun x -> [ x ]) alphabet in
  let rec aux = function
    | Eps | Star _ -> [ [] ]
    | Base x -> [ [ x ] ]
    | Joker -> alphabet
    | Concat (e, e') -> product (aux e) (aux e')
    | Alt (e, e') -> aux e @ aux e'
  in
  if is_finite e then Some (aux e) else None

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
