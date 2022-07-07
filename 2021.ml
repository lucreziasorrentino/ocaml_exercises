(*FEBBRAIO *)
(*1. Scrivere un programma con una funzione filquad: int list → int
list → int list, tale che filquad ints quads restituisca una lista
contenente, in qualsiasi ordine, tutti e solo gli elementi della lista ints
il cui quadrato è presente in quads. Ad esempio, la funzione, applicata
alla lista [1;2;3;4;5] e [9;25;10;4] riporterà una lista contenente gli
elementi 2, 3 e 5.
Scrivere diverse versioni della funzione:
(a) una che non utilizzi la funzione List.filter e implementi un algoritmo ricorsivo;
(b) una che non utilizzi la funzione List.filter e implementi un algoritmo iterativo;
(c) una che utilizzi la funzione List.filter. *)
let rec filquad_a ints quads = match ints with
    []-> []
    |x::rest -> if (List.mem (x*x) quads) then x:: filquad_a rest quads
                else filquad_a rest quads

let filquad_b inits quads = 
    let rec aux i q = match i with 
        []-> []
        |x::rest -> if (List.mem (x*x) q) then x:: aux rest q
                else aux rest q
    in aux inits quads
(*filquad_b [1;2;3;4;5] [9;25;10;4];; *)

let esiste q i = (List.mem (i*i) q)

let filquad_c inits quad = 
    List.filter (esiste quad) (inits)

(*2. In un albero etichettato da interi, il peso di un ramo è la somma delle
etichette dei nodi che compaiono su quel ramo.
Definire un tipo 'a ntree per la rappresentazione di alberi n-ari e una
funzione pesi: 'a ntree → 'a list che, applicata a un albero t, riporti 
una lista contentente tutti i pesi di tutti i rami che vanno dalla radice
a una foglia in t *)
type 'a ntree = Tr of 'a * 'a ntree list 

let sum x l = 
    x+ List.hd l ;;

let rec pesi Tr(x,tlst)= match tlst with
    []-> [x]
    |t::rest -> List.map (function y -> y+t) (from_list rest)
                 and from_list = function  
                       []-> [] (*ho finito di vedere tutti i fratelli *)
                       |t::rest -> pesi t @ from_list rest