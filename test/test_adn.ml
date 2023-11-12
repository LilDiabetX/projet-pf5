open Adn;;

assert(explode "" = []);;
assert(explode "Hello!" = ['H';'e';'l';'l';'o';'!']);;

assert(base_of_char 'A' = A);;
assert(base_of_char 'C' = C);;
assert(base_of_char 'T' = T);;
assert(base_of_char 'G' = G);;
assert(base_of_char '.' = WC);;

assert(dna_of_string "GTAA..CT" = [G;T;A;A;WC;WC;C;T]);;
assert(dna_of_string "" = []);;
assert(dna_of_string "ZxY" = [WC;WC;WC]);;

assert(string_of_dna [G; T; A; A; WC; WC; C; T] = "GTAA..CT");;
assert(string_of_dna [] = "");;

assert(cut_prefix [1; 2; 3] [1; 2; 3; 4] = Some [4]);;
assert(cut_prefix [1; 2; 3; 4] [1; 2; 3; 4] = Some []);;
assert(cut_prefix [1; 2; 0] [1; 2; 3; 4] = None);;
assert(cut_prefix [A; G; T] [A; G; T; C; A] = Some ([C; A]));;
assert(cut_prefix [] [1; 2; 3] = Some ([1;2;3]));;
assert(cut_prefix [1; 2; 0] [] = None);;
assert(cut_prefix [] [] = Some []);;