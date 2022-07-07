(*GRAFI*)
(** successori in un grafo orientato **)
(* successori : 'a -> 'a graph -> 'a list  *)
(* successori x g = lista dei successori di x in g (orientato) *)
let rec successori nodo grafo = match grafo with
    [] -> []
    |(x,y)::rest -> if x = nodo then y::successori nodo rest
                    else successori nodo rest

(** vicini in un grafo non orientato **)
(*  vicini : 'a -> 'a graph -> 'a list  *)
(* vicini x g = lista dei vicini di x in g (non orientato) *)
let rec vicini nodo grafo = match grafo with
    []-> [] (*se il grafo non ha più archi non hai più nodi da mettere tra i vicini*)
    |(a,b)::rest -> if (a=nodo) then b:: vicini nodo rest  (*se il grafo ha archi, controlla che un nodo sia vicino di nodo se è presente nell'arco *)
                    else if (b=nodo) then a:: vicini nodo rest
                        else vicini nodo rest

(*visita in profondità depth: 'a graph -> 'a -> 'a list *)
let depth graph start = 
    let rec search visited lstst = match sltst with
        []-> visited (*se sono qui ho finito di visitare tutti i nodi del grafo e riporto quindi la lista con tutti i nodi *)
        |n::rest-> if List.mem n visited then search visited rest (*se il primo nodo della lista l'ho già visitato vado avanti sui fratelli *)
                    else search (n::visited) ((successori n graph) @ rest) (*se non avevo ancora visitato il nodo lo aggiungo alla lista visited e vado avanti sui successori *)
    in search [] [start] 

(** Esempio: cercare se dal nodo start e' raggiungibile un nodo che soddisfa il predicato p. 
Riportare tale nodo, se la ricerca ha successo (grafo orientato) **)
(* search_node: 'a graph -> 'a -> ('a -> bool) -> 'a *)
let search_node grafo start p =
    let rec search visited nodi = match nodi with
        []-> failwith "niente nodo" (*grafo è finito, non hai trovato quello che cercavi *)
        |n::rest -> if (List.mem n visited) then search visited rest 
                    else if (p n) then n
                        else search (n::visited) ((successori n grafo) @ rest)
    in search [] [start]

(* NON SO SE E' GIUSTO: ricerca di un cammino in un grafo, a partire dal nodo start fino a un nodo che soddisfa 
il predicato p. Viene riportata una lista che rappresenta il cammino, o sollevata un'eccezione 
(grafo orientato) search_path : 'a graph -> 'a -> ('a -> bool) -> 'a list*)
let search_path grafo start p = 
    let rec search visited nodi = match nodi with
        []-> failwith "non riporto nulla perchè nessun nodo soddisfa p"
        |n::rest -> if List.mem n visited then search visited rest
                    else if (p n) then [n]
                        else n:: search (n::visited) ((successori n grafo) @ rest)
    in search [] [start]

(*Versione per grafo non orientato e voglio il cammino restituito*)
let path grafo start p = 
    let rec path_node visited nodo = 
        if (List.mem nodo visited) then failwith "non c'è, prova con i fratelli"
        else if p nodo then [nodo]
            else nodo:: path_list (nodo::visited) (vicini nodo grafo) (*controllo i miei discendenti *)
    and path_list visited lista= match lista with
        []-> failwith "non c'è tra i fratelli nessuno, hai stra perso"
        |x::rest -> try path_node visited x (*su x e i suoi discendenti non ho trovato nulla *)
                    with _-> path_list visited rest (*controllo i fratelli *)
    in path_node [] start


(*1. Scrivere una funzione test_connessi: ’a graph -> ’a -> ’a -> bool che, dato un grafo orientato G e 
due nodi N e M, determini se esiste un cammino da N a M. La funzione riporterà un booleano (non un cammino). *)
let test_connessi grafo n m =
    let rec test visisted lista = match lista with 
        []-> false (*non ci sono più successori di n *)
        |x::rest -> if List.mem x visited then test visited rest
                    else (x=m) || test (x::visited) ((successori x grafo) @ rest)
    in test [] [n]

(*2. Scrivere una funzione esiste_ciclo: ’a graph -> ’a -> bool che, dato un grafo orientato G e un nodo N, 
determini se esiste un ciclo su N (cioè un cammino da N a N che contenga almeno un arco). La funzione riporterà
un booleano (non un cammino). *)
let esiste_ciclo grafo n = 
    let rec ciclo visisted nodi = match nodi with
        []-> false 
        |x::rest-> if (List.mem x visited) then ciclo visited rest (*controllo se ci sono cicli tra i figli di n *)
                    else x=n ||  (ciclo (x::visisted) (successori x grafo)@rest)
    in ciclo [] (successori n grafo) (*parto a cercare dai discendenti di n e non dai fratelli che tanto non mi interessano *)

(*3. Scrivere una funzione ciclo: ’a graph -> ’a -> ’a list che, dato un
grafo orientato G e un nodo N, riporti, se esiste, un ciclo su N, altrimenti
sollevi un’eccezione (in questo caso la funzione deve riportare una lista di nodi). *)

let ciclo grafo n = 
    let rec from_node visited nodi = match nodi with
        [] -> failwith "ciclo non trovato"
        |x::rest -> if (List.mem x visited) then failwith "ciclo non trovato"
                    else if (n=x) then [x]
                         else x:: from_list (x::visited) (successori x grafo)
                    and from_list visitati lista = match lista with
                        []-> failwith "ciclo non trovato"
                        |l::rest -> try from_node visitati l
                                    with _-> from_list visitati rest
    in n:: from_node [] (successori n)

(*4. Un grafo non orientato è connesso se, per ogni coppia di nodi distinti N e M, esiste un cammino da N a M. 
Si definisca un tipo di dati ’a graph (diverso da quello dato all’inizio di questo gruppo di esercizi) per la
rappresentazione di grafi mediante due componenti: lista di nodi e lista di
archi, e scrivere una funzione grafo_connesso: ’a graph -> bool che,
dato un grafo non orientato G, determini se G è connesso.
Si noti che, per controllare se un grafo non orientato con nodi [n1;n2;....nk]
è connesso, basta controllare se n1 è connesso a n2, poi se n2 è connesso
a n3 (quindi n1 sarà connesso anche a n3), poi se n3 è connesso a n4, ecc.
Oppure, equivalentemente, si può controllare se n1 è connesso a n2, a n3,
a n4,... e basta. *)
type 'a graph = 'a list * ('a*'a) list ese. grafo = ([1,2,3], [(1,2);(2,3)])
let connesso (a,b) = 
    let rec from_node visited n = match n with
        if(List.mem n visited) then from_list (visited) (vicini n (a,b))
        else from_list (n::visited) (vicini n (a,b))
        and from_list visited = function
            []-> true
            |l::rest-> from_node 



    in from_node [] (vicini (a,b) List.hd a)










(*devo definire una vicini ma che scorra fino a finire la lista, se non finisce vuol dire che non è connesso, cioè tutti i nodi in visited devono essere quelli del grafo *)
let grafo_connesso (nodi,archi) = 
    let rec verifica tutti visited lista = match lista with
        []-> List.length tutti = List.length visited 
        |n::rest-> if (List.mem n visited) then  verifica tutti visited rest
                    else verifica tutti (n::visited) ((successori n)@rest)
    in nodi [] [List.hd nodi]    


(*6. (Dal compito d’esame di febbraio 2009). Sia data la seguente definizione
di tipo per rappresentare grafi orientati:
type ’a graph = ’a list * (’a * ’a) list
In altre parole, un grafo (orientato) è rappresentato da una coppia: il
primo elemento è una lista che rappresenta l’insieme dei nodi del grafo, il
secondo elemento è una lista di coppie, ciascuna delle quali rappresenta
un arco del grafo. Si assume che la lista dei nodi sia senza ripetizioni. *)
type ’a graph = ’a list * (’a * ’a) list

(*(a) Scrivere una funzione cammino: ’a graph -> ’a list -> ’a -> ’a-> ’a list che, 
dato un grafo G, una lista L senza ripetizioni e due
nodi n e m di G, riporti, se esiste, un cammino da n a m che passi solo
per nodi contenuti in L e per ciascuno di essi esattamente una volta.
Se un tale cammino non esiste, la funzione solleverà un’eccezione.
Suggerimento: adattare l’algoritmo di ricerca di un cammino in modo
tale che dalla lista L vengano via via eliminati i nodi già incontrati. Si
noti che in tal modo non è necessario memorizzare i nodi già visitati,
dato che la lista L stessa serve ad evitare i cicli *)
let rec successori_2 nodo (lnodi, larchi) = match larchi with
    [] -> []
    |(a,b)::rest -> if (a=nodo) then b:: successori_2 nodo (lnodi, rest)
                    else successori_2 nodo (lnodi, rest)
let rec rimuovi x lst= match lst with
    []-> []
    |y::rest-> if (x=y) then rest
                else y:: rimuovi x rest

let cammino grafo lista n m =
    let rec cammino_nodi lst nodi = match nodi with
        []-> failwith "fail"
        |x::rest -> if (List.mem x lst) then x:: cammino_lista (rimuovi x lst) (rest)
                    else x:: cammino_lista lst rest
    and cammino_lista lstsenza listaa nodil = match nodil with (*serve a vedere i fratelli *)
        []-> failwith "non esiste"
        |x::rest -> try cammino_nodi lst cammino_nodi listaa (successori_2 x grafo)
                    with cammino_lista lstsenza listaa rest 
    in cammino_nodi lista (successori_2 n grafo)

(*7. (Dal compito d’esame di luglio 2009). Si considerino le seguenti dichiarazioni di tipo, 
per la rappresentazione di colori e associazioni di colori:
type col = Rosso | Giallo | Verde | Blu
type ’a col_assoc = (col * ’a list) list
Scrivere un programma con una funzione colori_alterni: ’a graph ->
’a col_assoc -> ’a -> ’a -> ’a list che, dato un grafo orientato,
un’associazione di colori e due nodi del grafo start e goal, riporti – se
esiste – un cammino a colori alterni da start a goal, nel grafo dato. Se
un tale cammino non esiste, verrà sollevata un’eccezione. (Si vedano gli
esercizi 14 del Gruppo 8 e l’esercizio 12 del Gruppo 11, che propongono
lo stesso problema per gli alberi binari ed n-ari). *)
let rec col
let colori_alterni grafo col start goal = (*sbagliato il pattern matching dopo riguardalo *)
    let rec colori_node visited primo nodo = match nodo with
        []-> failwith "na"
        |n::rest-> if (List.mem nodo visited) then failwith "na"
                    else if (colore n col <> colore primo col)&&(n <> goal) then n:: colori_node (n::visited) nodo (successori n)
                            else if ((n = goal)&&(colore n col <> colore primo col)) then [n]
                                 else colori_list (n::visited) primo (rest)
            and colori_list visitati primo = function
                []-> failwith "na"
                |l::rest-> try -> colori_node visited primo l
                            with _ -> colori_list visited primo rest
    in start :: colori_node [] [start] (successori start)

(*8. (Dal compito d’esame di settembre 2009). Scrivere un programma con una
funzione connessi_in_glist: ’a graph list -> ’a -> ’a -> bool
che, data una lista di grafi orientati [G1,G2,...,Gn] e due elementi b
e c, determini se b e c sono nodi diversi tali che, per qualche grafo Gi
appartenente alla lista [G1,G2,...,Gn], esiste un cammino da b a c o
viceversa (da c a b). *)
let cammino grafo b c = 
    let rec from_node visited lista = match lista with
        []-> false
        |n::rest-> (n=c && not List.mem n visited) || from_node (n::visited) ((successori n)@rest)
    in from_node [] [successori b]

let rec connessi_in_glist listag b c = match listag with
    []-> false (*cerca se esiste un grafo con b e c perchè è li che andrò a cercare il cammino*)
    |g::rest-> if (List.mem b g) && (List.mem c g) then (cammino g b c) || (cammino g c b)
                else connessi_in_glist rest b c

(*9. (Dal compito d’esame di febbraio 2010). Scrivere un programma con
una funzione cammino_con_nodi: ’a graph -> ’a -> ’a list -> ’a
list che, dato un grafo orientato G, un nodo N di G e una lista L senza ripetizioni, 
restituisca, se esiste, un cammino senza cicli che, partendo
da N, contenga tutti i nodi di L (in qualsiasi ordine) ed eventualmente
anche altri nodi. Se un tale cammino non esiste, il programma solleverà
un’eccezione.
Ad esempio, se G è il grafo rappresentato da [(1, 2); (1, 3); (1, 4);
(2, 6); (3, 5); (4, 6); (6, 5); (6, 7); (5, 4)] e L è la lista [2;5], 
la ricerca a partire dal nodo 1 restituirà il cammino [1; 2; 6; 5].
Se la lista L è [2; 6; 3], la ricerca a partire dal nodo 1 fallirà perché
non esistono cammini in G che a partire da 1 tocchino tutti i nodi di L.*)
let cammino_con_nodi grafo n lista = 
    let rec from_node visited nodo lst =
        if (Lista.mem n visited) then failwith "na"
        else if (Lista.length lst = 0) || (Lista.mem n lst) then n:: from_list (n::visited) (rimuovi n lst) (successori nodo)
            else n :: from_list (n::visited) lst (successori nodo)
        and from_list visitati lst = function
            []-> []
            |l::rest->  try from_node visitati lst l
                        with _ -> from_list visitati lst rest
    in from_node [] n lista

(*11. (Dal compito d’esame di settembre 2010). Scrivere una funzione
cammino_di_primi: int graph -> int-> int -> int list
che, applicata a un grafo (orientato) di interi g e interi start e goal riporti,
se esiste, un cammino in g da start a goal costituito soltanto da numeri primi. *)
let rec successori n g = match g with
    [] -> []
    |(a,b)::rest -> if (a=n) then a:: successori n rest
                    else successori n rest

let primo n =
    let rec primi n i =
        if n>=i then true
        else if (n mod i <> 0) then false
             else primi n (i+1)
    in primi n 2

let cammino_di_primi g start goal = 
    let rec from_node visited n =
        if (List.mem n visited) || not (primo n) then failwith ""
        else if (n= goal) then [n]
        else n:: from_list (n::visited) (successori n g)
            and from_list visited = function
                [] -> failwith "na"
                |l::rest -> try from_node visisted l
                            with _-> from_list visited rest
    in from_node [] start 


(*13. (Dal compito d’esame di giugno 2011). Definire una funzione 
path_n_p: ’a graph -> (’a -> bool) -> int -> ’a -> ’a list, che, applicata
a un grafo orientato g, un predicato p: ’a -> bool, un intero non negativo n 
e un nodo start, riporti, se esiste, un cammino non ciclico da
start fino a un nodo x che soddisfa p e che contenga esattamente n nodi
che soddisfano p (incluso x). La funzione solleverà un’eccezione se un tale
cammino non esiste.
Ad esempio, sia pari: int -> bool definito in modo tale che pari x è vero se 
e solo se x è pari e g = [(1,3);(2,6);(3,4);(3,5);(3,6);(4,2);
(4,5);(5,4);(6,5)]. Allora, path_n_p g pari 2 1 può avere uno dei
valori seguenti: [1;3;4;2], [1;3;5;4;2], [1;3;6;5;4] *)
exception NotFound
let rec path_n_p g p n start = 
    let rec from_node visited i nodo=
        if (List.mem nodo visited) then raise NotFound
        else if not (p nodo) then raise NotFound
            else if (i=0) then [nodo] 
                 else nodo:: from_list (nodo::visited) (i-1) (successori nodo g) 
                and from_list visited i= function 
                    []-> raise NotFound
                    |l::rest -> try from_node visited i l
                                with NotFound -> try from_list visited i rest
    in from_node [] n nodo


