open Adn;;

(* Tests de la fonction explode*)
assert(explode "" = []);;
assert(explode "Hello!" = ['H';'e';'l';'l';'o';'!']);;

(* Tests de la fonction base_of_char*)
assert(base_of_char 'A' = A);;
assert(base_of_char 'C' = C);;
assert(base_of_char 'T' = T);;
assert(base_of_char 'G' = G);;
assert(base_of_char '.' = WC);;

(* Tests de la fonction dna_of_string*)
assert(dna_of_string "GTAA..CT" = [G;T;A;A;WC;WC;C;T]);;
assert(dna_of_string "" = []);;
assert(dna_of_string "ZxY" = [WC;WC;WC]);;

(* Tests de la fonction string_of_dna*)
assert(string_of_dna [G; T; A; A; WC; WC; C; T] = "GTAA..CT");;
assert(string_of_dna [] = "");;

(* Tests de la fonction cut_prefix*)
assert(cut_prefix [1; 2; 3] [1; 2; 3; 4] = Some [4]);;
assert(cut_prefix [1; 2; 3; 4] [1; 2; 3; 4] = Some []);;
assert(cut_prefix [1; 2; 0] [1; 2; 3; 4] = None);;
assert(cut_prefix [A; G; T] [A; G; T; C; A] = Some ([C; A]));;
assert(cut_prefix [] [1; 2; 3] = Some ([1;2;3]));;
assert(cut_prefix [1; 2; 0] [] = None);;
assert(cut_prefix [] [] = Some []);;

(* Tests de la fonction first_occ*)
assert(first_occ [1; 2] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([1; 1], [3; 4; 1; 2]));;
assert(first_occ [1; 1] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([], [1; 2; 3; 4; 1; 2]));;
assert(first_occ [1; 3] [1; 1; 1; 2; 3; 4; 1; 2] = None);;

(* Tests de la fonction slices_between*)
assert(slices_between [3; 3] [4; 4] [1; 1; 2; 3; 3; 1; 4; 1; 2] = []);;
assert(slices_between [1; 2] [4; 1] [1; 1; 2; 3; 2; 1; 4; 1; 2] = [[3; 2 ;1]]);;
assert(slices_between [A] [G] [A; C; T; G; G; A; C; T; A; T; G; A; G] = [[C; T]; [C; T; A; T]; []]);;

(* Test de la fonction cut_genes*)
let strand = dna_of_string "ATGCCTGGGCATTGAGATCATTGGCACCCTGCATAAGATGTGTGACTGTAGAGCTCTTCCTGACCATGCATAAAGAATG.CCAATGGCACAGCTGGTATC..TTTGCCATAAATGGCTCCTGGTGGAGCTGATAGTCACTTTCCATAATTAATGCATGGTGGTGGAGTTATTCTTGACTTTCCATAA";;
let genes = List.map (string_of_dna) (cut_genes strand);;
assert(genes = ["CCTGGGCATTGAGATCATTGGCACCCTGCA";"TGTGACTGTAGAGCTCTTCCTGACCATGCA";".CCAATGGCACAGCTGGTATC..TTTGCCA";"GCTCCTGGTGGAGCTGATAGTCACTTTCCA";"CATGGTGGTGGAGTTATTCTTGACTTTCCA"]);;

(* Tests de la fonction consensus*)
assert(consensus [1; 1; 1; 1] = Full 1);;
assert(consensus [1; 1; 1; 2] = Partial (1, 3));;
assert(consensus [1; 1; 2; 2] = No_consensus);;