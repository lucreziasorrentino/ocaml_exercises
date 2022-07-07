(* materiale lezione
let rec foglie_in_lista lista= function
    Empty-> true
    |Tr(x,Empty,Empty)-> List.exists(function y -> x=y) lista (* oppure List.mem x lista *)
    |Tr(_,t1,t2)-> foglie_in_lista lista t1 && foglie_in_lista lista t2


let rec funzione lista(Tr(x,tlist))= 
    if tlist= []
    then List.mem x lista 
    else flist lista tlist 
    and flist lista= function
        []-> true
        |t::rest-> funzione lista t && flat lista rest

    (*flist: 'a list->'a ntree list-> bool *)
*)
(* #use "08esercizi.ml";;  *)
(* questo type non si usa:
type 'a tree =
    Leaf of 'a
  | One of 'a * 'a tree
  | Two of 'a * 'a tree * 'a tree*)

(*Calcolo dimensione albero binario (e NON altezza): conta i nodi dell'albero *)
(*let rec size0 t = match t with
    Leaf _ -> 1
    |One (_, t1) -> 1 + (size0 t1)
    |Two (_, t1, t2) -> 1 + (size0 t1) + (size0 t2) ;; *)




type 'a tree =
 Empty
| Tr of 'a * 'a tree * 'a tree;;
(Tr(1, Tr(2, Tr(4,Empty,Empty), Empty),Tr(3, Tr(5, Tr(6,Empty,Empty), Tr(7,Empty,Empty)),Empty)))

let rec size t = match t with
    Empty -> 0
    |(_, t1, t2) -> 1 + (size t1) + (size t2) ;;*)

(*Calcolo se l'albero è vuoto *)
let is_empty = function
    Empty -> true
    | _ -> false;;

(*ESERCIZI *)

(*(a) reflect : ’a tree -> ’a tree. Applicata a un albero binario, ne costruisce l’immagine riflessa. 
Ad esempio, i due alberi sotto rappresentati sono uno l’immagine riflessa dell’altro 
(• rappresenta l’albero vuoto). *)
let rec reflect t= match t with
    Empty -> Empty
    |Tr(x, t1, t2) -> Tr(x, reflect t2, reflect t1)

(*(b) fulltree : int -> int tree. La funzione, applicata a un intero n, riporta un albero binario completo 
di altezza n, con i nodi etichettati da interi
come segue: la radice è etichettata da 1, i figli di un nodo etichettato da k
sono etichettati da 2k e 2k + 1. *)
let fulltree n = 
    let rec aux x i
        if n = 0 then Empty
        else Tr(i, aux (x-1) (2*i), aux (x-1) (2*i +1))
    in aux n 1

(*(c) balanced: ’a tree -> bool, determina se un albero è bilanciato (un albero è bilanciato se per ogni nodo n,
le altezze dei sottoalberi sinistro e destro di n differiscono al massimo di 1). *)
let rec height t = match t with
    Empty -> 0
    |Tr(_, t1, t2) -> 1 + max (height t1) (height t2) 

let rec balanced t = match t with
    Empty -> true
    |Tr(_, t1, t2) ->balanced t1 && balanced t2 && abs (height t1 - height t2) <= 1

(*(d) preorder, postorder, inorder, tutte di tipo ’a tree -> ’a list. Dato un albero t, le funzioni riportano 
la lista dei nodi di t, nell’ordine in cui sarebbero visitate secondo gli algoritmi di visita, rispettivamente,
in preordine, postordine e simmetrica.*)
let preorder t = match t with
    Empty -> []
    |Tr(x,t1,t2) -> x:: (preorder t1 @ preorder t2)

let postorder t = match t with
    Empty -> []
    |Tr(x,t1,t2) -> postorder t1 @ ((postorder t2) @ [x])

let inorder t = match t with
    Empty -> []
    |Tr(x,t1,t2) -> (inorder t1) @ (x :: (inorder t2))

(*(e) balpreorder e balinorder, entrambe di tipo ’a list -> ’a tree. Data
una lista lst, costruiscono un albero bilanciato con nodi etichettati da
elementi di lst, in modo tale che
preorder (balpreorder lst) = lst
inorder (balinorder lst) = lst
(utilizzare take e drop) 
take n lista= primi n elementi di lista, o lista stessa se non ce ne sono abbastanza
drop n lst = lista che si ottiene da lst togliendone i primi n elementi*)
let rec take n lista = match lista with
    []-> []
    |x::rest -> if n<=0 then []
                else x:: (take (n-1) rest)

let rec drop n lista = match lista with
    []-> []
    |x::rest as lst -> if n<=0 then lst
                else (drop (n-1) rest)

let rec balpreorder lst = match lst with
    []-> Empty
    |x::rest -> let l = (List.length rest)/2
                in Tr(x, Tr(take 1 rest, balpreorder (take l rest) ,balpreorder (drop l rest)))

let rec balinorder lst = match lst with (*è sbagliato *)
    []->
    |x::rest -> let l= (List.length rest)/2
                in Tr(balinorder (take l rest), x , balinorder (drop l rest))

(*3. foglie_in_lista: ’a list -> ’a tree -> bool, che, data una list lst e
un albero binario t, determini se ogni foglia di t appartiene a lst. (Una foglia
è rappresentata da un valore della forma Tr(x,Empty,Empty))*)
let rec foglie_in_lista lst t = match t with
    Empty -> true 
    |Tr(x,Empty, Empty) -> List.mem x lst
    |Tr(x,t1,t2) -> (foglie_in_lista lst t1) && (foglie_in_lista lst t2)

(*con gli alberi  n-nari *)
type 'a ntree = Tr of 'a * 'a ntree list
let rec foglie_in_listan lista (Tr(x,tlst)) = match tlst with
    []-> List.mem lista x
    |_ -> List.for_all (foglie_in_listan lista) tlst 
(*variante con mutua ricorsione *)
let rec filmr lista (Tr(x,tlst)) = match tlst with
    []-> List.mem x lista
    |_ -> film lista tlst 
        and film lista = function
            []-> true
            |t::rest -> filmr lista t && film lista rest
(*4. num_foglie: ’a tree -> int che, applicata a un albero binario, riporti il
numero di foglie dell’albero. *) 
let rec num_foglie t =
    Empty-> 0
    |Tr(x,Empty,Empty)-> 1
    |Tr(x,t1,t2) -> num_foglie t1 + num_foglie t2

let rec num_foglie_nari (Tr(x,tlst)) = match tlst with
    []-> 1
    |_ -> from_list tlst
        and from_list = function
        []-> 0
        |t::rest -> num_foglie_nari t + from_list rest 

(*5. Una lista di booleani L può determinare un sottoalbero di un albero binario:
quello che si ottiene, a partire dalla radice, scendendo al figlio sinistro per ogni
true nella lista, al figlio destro per ogni false. Se la lista è più lunga del ramo
che si ottiene da essa, allora il sottoalbero determinato da L è l’albero vuoto.
Si consideri ad esempio l’albero completo rappresentato sopra per l’esercizio 2b.
La lista [true;false;false] determina il sottoalbero che ha radice 11. La
lista [false;true] determina il sottoalbero con radice 6. Liste con più di 3
elementi determinano l’albero vuoto.
Scrivere una funzione segui_bool: bool list -> ’a tree -> ’a che, data
una lista L di booleani e un albero binario T, riporti la radice del sottoalbero
di T determinato da L, se questo non è vuoto, un errore altrimenti.
Ad esempio, la funzione, applicata alla lista [true;false;false] e all’albero
rappresentato per l’esercizio 2b riporterà 11. Applicata alla lista [false;true]
e allo stesso albero, riporterà 6. Riporterà un errore se la lista ha più di 3
elementi. *)
let rec segui_bool list t = match t with
    Empty-> failwith "lista troppo lunga"
    |Tr(x,t1,t2)-> if (List.length = 0) then x 
                    else if (List.hd) then segui_bool (List.tl list) t1
                         else segui_bool (List.tl list) t2

(*6. Se T e’ un albero binario etichettato da numeri interi, il costo di una foglia N
di T è la somma di tutti i nodi che si trovano sul ramo che va dalla radice di
T a N. Scrivere una funzione foglia_costo: int tree -> (int * int) che,
dato un albero binario di interi, restuisca l’etichetta e il costo di una delle foglie
più costosa dell’albero. *)
let summ x (a,b) = (a,b+x)
let rec foglia_costo t = match t with
    Empty-> failwith "qui non ci finisci mai"
    |Tr(x, Empty, Empty)-> (x,x) (*foglia*)
    |Tr(x, t, Empty) | Tr(x, Empty, t) -> x+ snd (foglia_costo t)
    |Tr(x,t1,t2)-> summ x (max (snd foglia_costo t1) (snd foglia_costo t2))

let rec foglia_costo_prof = function
    Empty-> failwith "qui non ci finisci mai"
    |Tr(x, Empty, Empty)-> (x,x) (*foglia*)
    |Tr(x, t, Empty) | Tr(x, Empty, t)  -> let (a,b)= foglia_costo_prof t
                                            in (a, x+b)
    |Tr(x,t1,t2)-> let (a1,b1)= foglia_costo_prof t1 in 
                   let (a2,b2)= foglia_costo_prof t2 in 
                   if (b1>b2) then (a1,b1+x)
                   else (a2,b2+x)

(*7. Definire una funzione foglie_costi: int tree -> (int * int) list che,
applicata a un albero binario T etichettato da interi, riporti una lista di coppie,
ciascuna delle quali ha la forma (f,n), dove f è l’etichetta di una foglia in T e
n il costo di tale foglia (dove il costo di una foglia è definito come nell’esercizio
precedente). *)
let foglie_costi t = 
    Empty-> failwith "qui non ci finisci mai"
    |Tr(x, Empty, Empty)-> [(x,x)] (*foglia*)
    |Tr(x, t, Empty) | Tr(x, Empty, t)  -> let (a,b) = foglie_costi t
                                            in (a, b+x)
    |Tr(x,t1,t2)-> let (a1,b1) = foglie_costi t1 in
                let (a2,b2) = foglie_costi t2 in 
                (a1, x+b1)@(a2, x+b2) 

(*9. Definire una funzione max_common_subtree: string tree -> string tree
-> string tree, che, dati due alberi binari A e B, i cui nodi sono etichettati
da stringhe, costruisca il massimo sottoalbero comune a A e B, partendo dalla
radice: i nodi di tale sottoalbero avranno la stessa etichetta che hanno i nodi
corrispondenti in A e in B, se essi sono uguali; altrimenti, se il nodo x di A è
diverso dal corrispondente nodo di B (o se uno dei due nodi non c’è), il nodo
corrispondente a x nel massimo sottoalbero comune di A e B sarà una foglia
etichettata da "@".*)
let rec max_common_subtree t1 (Tr(a,b,c)) = match t1 with
    Empty-> if (a=Empty) then Empty else Tr("@", Empty, Empty) (*non si puo scrivere mi sa *)
    |Tr(x, Empty, Empty)-> if (x=a) then Tr(x, Empty, Empty)
                            else Tr("@", Empty, Empty)
    |Tr(x,t1,t2)-> if (x=a) then Tr(x, max_common_subtree t1 b, max_common_subtree t2 b)
                    else Tr("@", Empty, Empty)

let rec max_common_subtree_prof t1 t2 = match (t1,t2) with
    (Empty, Empty)-> Empty
    |(Empty,_)| (_,Empty) -> Tr("@", Empty, Empty)
    |(Tr(x1,y1,z1), Tr(x2,y2,z2)) -> if (x1=x2) then Tr(x1, max_common_subtree_prof y1 y2, max_common_subtree_prof z2 z2)
                                              else Tr("@", Empty, Empty)

(*10. (Dal compito d’esame di settembre 2011)
(a) Scrivere un predicato stessa_struttura: ’a tree -> ’a tree -> bool
che determini se due alberi binari hanno la stessa struttura (cioè se essi sono uguali quando 
si ignorano le rispettive etichette; ad esempio i tre alberi
rappresentati per l’esercizio successivo hanno tutti la stessa struttura). *)
let rec stessa_struttura t1 t2 = match (t1,t2) with
    (Empty, Empty)-> true
    |(Empty,_)| (_,Empty) -> false
    |(Tr(x1,y1,z1), Tr(x2,y2,z2)) -> (stessa_struttura y1 y2) && (stessa_struttura z1 z2)
    |_ -> false

(*(b) Una funzione f è un mapping da un albero binario t1 a un albero t2 se
l’applicazione di f alle etichette di t1 trasforma t1 in t2. In particolare,
perché possa esistere un mapping da t1 a t2, i due alberi devono avere la
stessa struttura, ma questa non è una condizione sufficiente. *)



(*11. (Dal compito d’esame di giugno 2011) Definire una funzione
path: (’a -> bool) -> ’a tree -> ’a list,
che, applicata a un predicato p: ’a -> bool e a un albero t: ’a tree, riporti,
se esiste, un cammino dalla radice a una foglia di t che non contenga alcun nodo
che soddisfa p. La funzione solleverà un’eccezione se un tale cammino non esiste *)

let rec path p t = match t with
    Empty -> failwith "non esiste"
    |Tr(x, Empty, Empty) -> if (not (p x)) then [x] else failwith "non questo cammino"
    |Tr(x, t1, t2) ->  if (not (p x)) then x:: (try (path p t1)
                                            with _ -> path p t2)
                        else failwith "non questo cammino"

(*12. (Riformulazione di un esercizio del compito d’esame di febbraio 2011) Sia data
la seguente dichiarazione di tipo: type ’a sostituzione: (’a * ’a tree) list 
Una sostituzione è una lista associativa che associa alberi a valori di tipo ’a.
L’applicazione di una sostituzione subst a un albero t è l’albero che si ottiene
da t sostituendo ogni foglia etichettata da x con l’albero xt associato a x in
subst, se subst associa a x un albero, e lasciandola immutata altrimenti.
Scrivere una funzione applica: ’a sostituzione -> ’a tree -> ’a tree che,
applicata a una sostituzione subst e un albero t riporti l’albero che si ottiene
applicando la sostituzione subst a t. *)
let rec applica s tree = match (s,tree) with
    ([],t)-> t
    |((a,b)::rest,Tr(x,t,Empty)) | ((a,b)::rest,Tr(x,Empty,t)) -> Tr(x, applica lista t, Empty)
    |((a,b)::rest,Tr(x,Empty,Empty))-> if a=x then b else Tr(x,Empty,Empty)
    |(lista, Tr(x,t1,t2))-> Tr(x, applica lista t1, applica lista t2)

(*13. (Dal compito d’esame di febbraio 2010). Si definisca un tipo di dati per la
rappresentazione di alberi binari e scrivere un programma con una funzione
path_coprente: ’a tree -> ’a list -> ’a list che, dato un albero A e
una lista di elementi dello stesso tipo dei nodi di A, restituisca, se esiste, un
ramo dell’albero dalla radice a una foglia che contenga tutti i nodi di L (in
qualsiasi ordine) ed eventualmente anche altri nodi. Se un tale cammino non
esiste, il programma solleverà un’eccezione. Si assuma che la lista L sia senza
ripetizioni.
Ad esempio, se l’albero è quello rappresentato per l’esercizio 6 e la lista è [3;6],
la funzione può restituire il ramo [0;6;6;3] oppure 0;6;4;3]. Se la lista è
[10], la funzione può restituire il ramo [0;10;2] oppure 0;10;5]. *)
let rec rimuovi x lst = match lst with 
    []->[]
    |y::rest -> if x=y then rest
                else rimuovi x rest

let rec path_coprente t lst = match t with
    Empty -> failwith "fallimento"
    | Tr(x,Empty, Empty) -> if (List.mem x lst && List.length=1) then [x] else failwith "fallimento"
    | Tr(x,t1,t2)-> x:: (try path_coprente t1 (rimuovi x lst) 
                        with _-> (path_coprente t2 (rimuovi x lst)))
                    


(*let rec path x tree: ricerca di un cammino fino a una foglia x *)
let rec path x t = match t with
    Empty-> failwith "ci finisci solo se tuo padre aveva un unico figlio (e non sei tu)"
    |Tr(y,Empty,Empty)-> if y=x then [y]
                        else failwith "foglia sbagliata"
    |Tr(y,t1,t2)-> y::(try path x t1
                        with _-> path x t2)


(*ESAME SETTEMBRE 2019
2. Se x è un'etichetta di uno o più nodi di un albero t, un discendente di x
in t è l'etichetta di un nodo che occorre in qualsiasi sottoalbero di t la cui
radice è etichettata da x.
Denire un tipo di dati α tree per la rappresentazione di alberi binari e
un programma con una funzione discendenti: α → α tree → α list
che, applicata a un valore x di tipo α e un albero t: α tree riporti una
lista contenente (in qualsiasi ordine e anche con ripetizioni) tutti i discendenti di x in t. 
Se nessun nodo dell'albero è etichettato da x, la funzione
riporterà la lista vuota. Suggerimento: denire una funzione di supporto labels: α tree → α list
tale che labels t = lista contenente tutte le etichette dei nodi di t (in
qualsiasi ordine e anche con ripetizioni). *)
type 'a tree =
Empty 
|Tr of 'a * 'a tree * 'a tree;;

let rec labels t = match t with (*normale visita di un albero *)
    Empty->[]
    |Tr(x,t1,t2)-> x:: ((labels t1)@(labels t2));;

let rec discendenti x t = match t with (*sbagliato, guarda lezione del 14/12 *)
    Empty -> failwith "x non c'è in t"
    |Tr(y,t1,t2)-> if (List.mem x (labels t)) then aux t  (*qui scorro labels e cerco x cosi se la trovo parto a creare la lista dei discendenti *)
                    else try discendenti x t1
                        with _-> discendenti x t2
                    and aux t = match t with 
                        Empty -> []
                        |Tr(y,Empty,Empty) -> [y]
                        |Tr(y,t1,t2) -> y:: (aux t1 @ aux t2)











