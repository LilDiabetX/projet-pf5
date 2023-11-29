type base = A | C | G | T | WC (* wildcard *)

type dna = base list



(*---------------------------------------------------------------------------*)
(*                               ECHAUFFEMENT                                *)
(*---------------------------------------------------------------------------*)


let string_of_base (b : base) : string =
  match b with
    A -> "A"
  | C -> "C"
  | G -> "G"
  | T -> "T"
  | WC -> "."


(* explode a string into a char list *)
let rec explode_aux str acc =
  if str = String.empty then List.rev_append acc []
  else explode_aux (String.sub str 1 (String.length str - 1)) ((String.get str 0)::acc)

let rec explode (str : string) : char list =
  explode_aux str []

(* conversions *)
let base_of_char (c : char) : base =
  if c = 'A' then A
  else if c = 'C' then C
  else if c = 'G' then G
  else if c = 'T' then T 
  else WC


let dna_of_string (s : string) : base list =
  List.map base_of_char (explode s)


let string_of_dna (seq : dna) : string =
  List.fold_left (fun x y -> x ^ y) "" (List.map string_of_base seq)


(*---------------------------------------------------------------------------*)
(*                                   SLICES                                  *)
(*---------------------------------------------------------------------------*)
(*
   Une {\em tranche} de $l = \langle x_1,\dots x_n\rangle$ est une sous-liste
   de $l$ de la forme $\langle x_i, \dots x_{j}$, o\`u $1 \leq i \leq j\leq n$.
 *)


(* if list = pre@suf, return Some suf. otherwise, return None *)
let rec cut_prefix (slice : 'a list) (list : 'a list) : 'a list option =
  match slice,list with
  |h1::t1,h2::t2 -> if h1 != h2 then None else cut_prefix t1 t2
  |h::t,[] -> None
  |[],_ -> Some list

(*
  cut_prefix [1; 2; 3] [1; 2; 3; 4] = Some [4]
  cut_prefix [1; 2; 3; 4] [1; 2; 3; 4] = Some []
  cut_prefix [1; 2; 0] [1; 2; 3; 4] = None
 *)


(* return the prefix and the suffix of the first occurrence of a slice,
   or None if this occurrence does not exist.
*)

let rec first_occ_aux slice list acc = 
  match cut_prefix slice list with
  |Some l -> Some (List.rev_append acc [],l)
  |None -> 
    match list with
    |[] -> None
    |h::t -> first_occ_aux slice t (h::acc)
  

let first_occ (slice : 'a list) (list : 'a list)
    : ('a list * 'a list) option =
  first_occ_aux slice list []
  
(*
  first_occ [1; 2] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([1; 1], [3; 4; 1; 2])
  first_occ [1; 1] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([], [1; 2; 3; 4; 1; 2])
  first_occ [1; 3] [1; 1; 1; 2; 3; 4; 1; 2] = None
 *)

let rec slices_between_aux start stop list acc = 
  match first_occ start list with
  |None -> List.rev_append acc []
  |Some (pre1,suf1) -> 
    match first_occ stop suf1 with
    |None -> List.rev_append acc []
    |Some (pre2,suf2) -> slices_between_aux start stop suf2 (pre2::acc)

let slices_between
          (start : 'a list) (stop : 'a list) (list : 'a list) : 'a list list =
  slices_between_aux start stop list []

(*
  slices_between [1; 1] [1; 2] [1; 1; 1; 1; 2; 1; 3; 1; 2] = [[1]; []; [2; 1; 3]]
 *)

let cut_genes (dna : dna) : (dna list) =
  slices_between [A;T;G] [T;A;A] dna

(*---------------------------------------------------------------------------*)
(*                          CONSENSUS SEQUENCES                              *)
(*---------------------------------------------------------------------------*)


type 'a consensus = Full of 'a | Partial of 'a * int | No_consensus

(* return (Full a) if all elements of the list are equal to a,
   (Partial (a, n)) if a is the only element of the list with the
   greatest number of occurrences and this number is equal to n,
   No_consensus otherwise. the list must be non-empty *)

let consensus_aux_1 list = 
  List.fold_left (fun acc x -> match List.assoc_opt x acc with |Some n -> (x,n+1)::(List.remove_assoc x acc) |None -> (x,1)::acc) [] list

let rec consensus_aux_2 list acc =
  match list with
  |[] -> Partial ((fst acc),(snd acc))
  |h::t -> if snd h < snd acc then consensus_aux_2 t acc else if snd h > snd acc then consensus_aux_2 t h else No_consensus

let consensus (list : 'a list) : 'a consensus =
  if list = [] then No_consensus
  else if List.length (consensus_aux_1 list) = 1 then Full (fst (List.hd (consensus_aux_1 list)))
  else consensus_aux_2 (List.tl (consensus_aux_1 list)) (List.hd (consensus_aux_1 list))

(*
   consensus [1; 1; 1; 1] = Full 1
   consensus [1; 1; 1; 2] = Partial (1, 3)
   consensus [1; 1; 2; 2] = No_consensus
 *)

(* return the consensus sequence of a list of sequence : for each position
   in the elements of ll, compute the consensus  of the set of values at this
   position  in the sequences. the lists must be of same length. if all lists
   are empty, return the empty sequence.
 *)

let consensus_sequence (ll : 'a list list) : 'a consensus list =
  failwith "À compléter"

(*
 consensus_sequence [[1; 1; 1; 1];
                     [1; 1; 1; 2];
                     [1; 1; 2; 2];
                     [1; 2; 2; 2]]
 = [Full 1; Partial (1, 3); No_consensus; Partial (2, 3)]
 *)
