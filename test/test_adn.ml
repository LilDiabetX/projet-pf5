open Adn;;

assert(explode "" = []);;
assert(explode "Hello!" = ['H';'e';'l';'l';'o';'!']);;

assert(base_of_char 'A' = A);;
assert(base_of_char 'C' = C);;
assert(base_of_char 'T' = T);;
assert(base_of_char 'G' = G);;
assert(base_of_char '.' = WC);;