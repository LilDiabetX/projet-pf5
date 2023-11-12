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
assert(dna_of_string "ZxY" = [WC;WC;WC])