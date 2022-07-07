(*Definire una funzione che, prendendo in input una lista di liste di interi e un intero x, 
aggiunga x in testa ad ogni lista. Utilizzare List.map.
Esempio: aggInTesta [[1;2];[2;3];[3;4]] 9 = [[9;1;2];[9;2;3];[9;3;4]]*)

let agg x lista = x::lista;;

let aggInTesta x lstdilst = (List.map (agg x) lstdilst) ;;


(*Implementare da zero due funzioni che prendono gli stessi parametri e restituiscono gli 
stessi risultati delle funzioni di libreria exists e for_all del modulo List *)

let rec esiste funz lista = match lista with
    []-> false
    |x::rest -> if (funz x) then true
                else esiste funz rest;;

let rec petuttiproprio funz lista = match lista with
    []-> true
    |x::rest -> if (funz x) then petuttiproprio funz rest
                else false;;

(*Definire una funzione che, prendendo in input una lista di liste di interi e un intero x, 
aggiunga x in testa ad ogni lista. Utilizzare List.map.
Esempio: aggInTesta [[1;2];[2;3];[3;4]] 9 = [[9;1;2];[9;2;3];[9;3;4]]*)

let aggsemplice x intero = (x, intero);;

let aggInTestasemplice x lista = (List.map (aggsemplice x) lista) ;;

(*utilizzo di List.sort: ('a -> 'a -> int) -> 'a list -> 'a list
cioè List.sort ha 2 parametri: 
1) ('a -> 'a -> int) cioè una funzione che prende due parametri dello stesso tipo e restituisce un intero
2) 'a list cioè una lista
restituisce una lista come quella passata per parametro, ma a seconda della funzione passata 
per parametro la ordina: cioè ordina in ordine crescente se ad esempio come funzione usi compare,
funzione che restituisce int negativo se primo elemento minore secondo, un int positivo se il primo è maggiore
e 0 se sono uguali #use "esercizi.ml";;*)
let cmp n m = if (n > m) then 1 else -1 ;;
let sorta lst = List.sort (cmp) lst ;;

(*compare : ('a -> 'a -> int) -> 'a list -> 'a list -> int *)
(*let comp lst1 lst2 = List.compare (cmp) lst1 lst2 ;;*)


(*Utilizzo di iter: iter : ('a -> unit) -> 'a list -> unit *)
let f x = ();;
let itera lst = List.iter f lst;; 


(*Utilizzo di find f lst che prende due parametri: 
1) una funzione che ha un parametro e restituisce un bool
2) una lista
restituisce il primo elemento della lista che soddisfa f
cioè è come exists ma restituisce l'elemento e non un semplice bool 
qui ho fatto un esempio in cui List.find mi trova il primo elemento in lista che è pari*)
let funz x = if (x mod 2 = 0) then true else false;; (*funzione che dice se x è pari *)
let findese lst = List.find funz lst;;

(*Utilizzo di filter f lst che prende 2 parametri:
1) una funzione che ha un parametro e verifica una certa proprietà restituendo un bool
2) una lista
restituisce una lista di tutti gli elementi della lista passata per parametro che soddisfano
la funzione passata per parametro
qui ho sfruttato la funzione funz sopra definita per trovare gli elementi pari in lista
 #use "esercizi.ml";; *)
let filterese lst = List.filter funz lst;;

(*Uso di List.map ma con 3 parametri in funzione ausiliaria *)
let auxmap x y n = (x,y, List.length n);;
let usomap lst x y= List.map (auxmap x y) lst;;
let usomap2 lst x y= List.map (function a -> (a, x::y)) lst;;

(*VISITA: lista nodi con un figlio *)
let rec nuf = function
    Empty | Tr(_, Empty, Empty) -> []
    |Tr(x,Empty,t) | Tr(x,t,Empty) -> x:: nuf t
    |Tr(x,t1,t2) -> (nuf t1) @ (nuf t2)

(*RICERCA: restituisce lista con ricerca di un ramo che ha foglia y *)
let rec ric x = function
    Empty -> failwith "non c'è"
    |Tr(y, Empty, Empty) -> if x=y then [y] else failwith "non c'è"
    |Tr(y,t1,t2) -> y:: try ric x t1
                        with _ -> ric x t2