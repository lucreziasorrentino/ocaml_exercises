(*ALBERI N-NARI *)
type 'a ntree = Tr of 'a * 'a ntree list

(*size (conta numero nodi)*)
let rec sumof lst = match lst with  (*somma gli interi in lista ese. sumof [1;2;3]= 6 *)
    []-> 0
    |x::rest -> x+ sumof rest;;

let rec size (Tr(x,tlst)) = 
    if tlst = [] then 1
    else 1 + sumof(List.map size tlst) (*a ogni elemento della tlst applica size cosi scorre tutto l'albero, poi avrà 1 + sumof[interi con varie size] *)

(*altezza albero *)
let rec height (Tr(x,tlst)) = match tlst with
    []-> 0
    |_-> 1+ hl tlst
        and hl = function
            []-> failwith "basta"
            |[t]-> height t
            |t::rest-> max (height t) (hl rest)

(*fattore di ramificazione: quanti figli ci sono al massimo? *)
let rec fdr (Tr(x,tlst)) = match tlst with
    []-> 0
    |_-> max (List.length tlst) (maxl (List.map fdr tlst))
(*con mutua ricorsione *)
let rec fdrmr (Tr(x,tlst)) =
    []-> 0
    |_-> max (rmr tlst) (List.length tlst) (*qui confronti numero di figli di x e il max numero di figli di ogni figlio = CONFRONTO TRA PADRE E FIGLI  *)
        and rmr = function
            []-> failwith "non ci finisci mai" (*qui ci finisci solo se rmr viene invocata da rmr stessa *)
            |[t]-> fdrmr t (* sei ad una foglia -> restituisce 0 come numero di figli *)
            |t::rest ->max (rmr rest) (fdrmr t) (*confronta il numero di figli di t con quello dei fratelli = CONFRONTO TRA FRATELLI *)

(*test di esistenza di un nodo y in un albero *)
let rec esistee (Tr(x,tlst)) y = match tlst with
   []-> x=y 
   |_-> x=y || (esistemr tlst y) (*qui controlli padre e affidi il controllo dei figli alla funz sotto *)
        and esistemr tlst y = match tlst with
            []-> false (*qui ci arrivi se hai visto tutti i fratelli e nessuno è true *)
            |t::rest -> (esistee t y) || (esistemr rest y) (*affidi il controllo del primo fratello e dei suoi figli sopra 
                                                            mentre tu ti occupi di passare sopra tutti i fratelli *)

(*let rec path x tree: ricerca di un cammino fino a una foglia x *)
let rec path x (Tr(y,tlst))= match tlst with
    (*mi occupo di t e dei figli *)
    []-> if x=y then [y] (*ho una foglia perchè se sono qui t non ha figli (cioè tlst figli è vuota) *)
        else failwith "non sono la foglia che cercavi"
    |_-> y:: pathmr x tlst (*non sono ad una foglia quindi devo continuare a scendere sui miei figli *) 
            and pathmr x = function    (*mi occupo dei fratelli e di mandare t alla funz principale *)
                []-> failwith "non ci sono cammini con quella foglia, ho guardato tutti i fratelli e i loro figli!"
                |t::rest -> t:: try path x t
                                with _-> pathmr x rest

(*ricerca di un cammino che soddisfa il predicato p *)
let rec sodd p (Tr(y,tlst))=
    (*mi occupo di t e dei figli *)
    if p y then [y] 
    else y:: soddmr p tlst
            and soddmr p = function    (*mi occupo dei fratelli e di mandare t alla funz principale *)
                []-> failwith "non ci sono cammini con quella foglia, ho guardato tutti i fratelli e i loro figli!"
                |t::rest -> t:: try sodd p t
                                with _-> soddmr p rest

(*ESERCIZI GRUPPO 09 *)
(*2. Nella visita in postordine degli alberi n-ari vengono prima visitati tutti
i sottoalberi, poi la radice. Nella visita simmetrica viene prima visitato
il sottoalbero sinistro, poi la radice, poi gli altri sottoalberi (se ve ne
sono). Implementare due funzioni postorder: ’a ntree -> ’a list e
inorder: ’a ntree -> ’a list che, dato un albero n-ario, riportino la
lista dei suoi nodi nell’ordine in cui sarebbero visitati secondo i due algoritmi di visita. *)
let rec postorder (Tr(x,tlst)) = match tlst with
    []-> [x]
    |_ -> from_list tlst @ [x]
                and from_list = function
                    [] -> []
                    |t::rest-> (postorder t @ from_list rest)

let rec inorder (Tr(x,tlst)) = match tlst with
    []-> [x]
    |t::rest -> [x]:: (inorder t  @ from_list rest)
                and from_list = function
                    [] -> []
                    |t::rest-> (postorder t @ from_list rest)

(*3. Scrivere una funzione foglie_in_lista: ’a list -> ’a ntree -> bool
che, data una lista lst e un albero n-ario t, determini se ogni foglia di t
appartiene a lst. *)
let rec foglie_in_lista lst (Tr(x,tlst)) = match tlst with
    []-> (List.mem x lst) 
    |_ -> (fratelli lst tlst) (*non serve scorrere anche t, lo fai sotto *)
        and fratelli lista = function
            []-> true
            |t::rest -> (foglie_in_lista lst t) && (fratelli lst rest) 

(*4. Scrivere una funzione num_di_foglie: ’a ntree -> int che, applicata
a un albero n-ario, riporti il numero di foglie dell’albero. *)
let rec num_di_foglie (Tr(x,tlst)) = match tlst with
    []-> 1 (*x è una foglia *)
    |_-> num_di_foglie_bis tlst
        and num_di_foglie_bis = function
            []-> 0 (*fine fratelli non vuol dire che sono arrivata ad una foglia *)
            |t::rest-> (num_di_foglie t) + (num_di_foglie_bis rest)



(*5. Una lista di interi non negativi L può determinare un sottoalbero di un
albero n-ario T: quello che si ottiene, a partire dalla radice, scendendo,
per ogni elemento n di L, al sottoalbero in posizione n nella lista dei
sottoalberi (se esiste) – si ricordi che la posizione degli elementi in una
lista si conta a partire da 0. Se la lista è più lunga del ramo cui essa
conduce, oppure se a qualche livello non esiste un numero sufficiente di
sottoalberi, allora L non determina alcun sottoalbero di T. 
Scrivere una funzione listaGuida: ’a list -> ’a ntree -> ’a, che,
data una lista L di interi e un albero n-ario T, riporti la radice del sottoalbero di T determinato da L, 
se L determina un sottoalbero di T, un errore altrimenti.*)
let rec prendi y tlst = (*restituisce elemento in posizione y in tlst *)
let rec listaGuida lista Tr(x,tlst) = match lista with   (*qui non si usa il solito schema perchè il cammino che cerchi è ben preciso, non devi guardare i fratelli di nessuno *)
    []-> x
    |y::rest -> try listaGuida (rest) (List.nth tlst y)
                with _-> failwith "non c'è"

let rec lista_guida lista Tr(x, tlst) = match lista with
    []-> x
    |l::rest -> lista_guida (rest) (take (Lista.hd lista) (tlst)) 

let rec take x fratelli = match fratelli with
    [] -> failwith "x troppo grande per fratelli"
    |f::rest-> if (x=0) then f
                else take (x-1) rest


(*6. Se T è un albero n-ario etichettato da numeri interi, il costo di una foglia
N di T è la somma di tutti i nodi che si trovano sul ramo che va dalla
radice di T a N. Scrivere una funzione foglia_costo: ’int ntree ->
(int * int) che, dato un albero n-ario di interi, restuisca l’etichetta e il
costo della foglia più costosa dell’albero. L’albero può anche avere diversi
nodi con la stessa etichetta. *)
let rec maxx (a,b) (c,d) =      (*definisco questa funzione per calcolare il massimo tra secondi membri di coppie *)
    if b>d then (a,b)
    else (c,d)

let rec fc (Tr(x,tlst))= match tlst with
    []-> (x, x) (*foglia*)
    |_-> let (a,b) = fcmr tlst    (*il risultato di fcmr x tlst mettilo nella coppia (a,b) *)
        in (a,b+x)                  (*la coppia (a,b) però è fatta in modo che b sia la somma del secondo membro di fcmr x tlst e di x *)
        and fcmr = function
            []-> failwith "tanto qui non ci finisci"
            |[t]-> fc t (*questo è l'ultimo fratello da controllare *)
            |t::rest-> maxx (fc t) (fcmr rest) (*prendo il massimo costo tra i fratelli*)

(*stessa domanda ma su alberi binari *)
let rec fcb t = match t with
    Empty -> 
    |Tr(x, y, Empty) | Tr(x, Empty, y)-> let (a,b)= (fcb y) in (a,b+x)
    |Tr(x, Empty, Empty) -> (x,x)
    |Tr(x, t1,t2) -> let (a,b) = maxx (fcb t1) (fcb t2)
                    in (a,x+b)


(*Esercizio 14/12/2021: lista discendenti di x  *)
let rec discendenti x Tr(y,tlst) = 
    if y=x then visita_alberi Tr(y,tlst)  (*fai la lista con tutti i successori con una visita degli alberi *)
    else discendenti_list x tlst
        and discendenti_list x lista = match lista with
            []->[]
            |t::rest -> (discendenti x t) @ (discendenti_list x rest) (*metto la concatenazione perchè tanto mi darà sempre [] *)


let rec labels = function
   Empty -> []
 | Tr(x, Empty, Empty) -> [x]
 | Tr(x, t1, t2) -> [x] @ labels t1 @ labels t2

let rec discendenti n = function
   Empty -> []
 | Tr(x, t1, t2) -> (if x=n then labels (Tr(x,t1,t2))
                     else discendenti n t1 @ discendenti n t2)


(*7. Definire una funzione tutte_foglie_costi: int ntree -> (int * int)
list che, applicata a un albero n-ario T etichettato da interi, riporti una
lista di coppie, ciascuna delle quali ha la forma (f,n), dove f è l’etichetta
di una foglia in T e n il costo di tale foglia (dove il costo di una foglia
è definito come nell’esercizio precedente). Anche in questo caso, l’albero
può anche avere diversi nodi con la stessa etichetta. *)
let rec summ x lista_coppie = match lista_coppie with (*somma x a ogni secondo elemento di ogni coppia *)
    []-> [] (*lista finita *)
    |(a,b)::rest-> (a, b+x) :: summ x rest 

let rec tutte_foglie_costi (Tr(x,tlst))= match tlst with
    []-> [(x, x)] (*x è foglia *)
    |_ -> summ (x) (List.flatten (from_list tlst))
            and from_list = function
                []-> [] (*finito: che parentesi? *)
                |t::rest -> List.flatten ((tutte_foglie_costi t) @ (from_list rest)) (*errore parentesi*)

let rec tutte_foglie_costi_prof (Tr(x,tlst))= match tlst with
    []-> [(x, x)] (*x è foglia *)
    |_-> List.map (function (a,y)-> y+x) (List.flatten (List.map tutte_foglie_costi_prof tlst))

(*8. (Dal compito d’esame di febbraio 2009).
Scrivere una funzione ramo_da_lista: ’a ntree -> ’a list -> ’a ->
’a list che, dato un albero T, una lista L senza ripetizioni e un’etichetta
k, riporti, se esiste, un ramo di T dalla radice a una foglia etichettata da k
che passi per tutti gli elementi di L esattamente una volta e contenga solo
nodi etichettati da elementi di L (in pratica, il cammino deve essere una
permutazione di L). Se un tale cammino non esiste, la funzione solleverà
un’eccezione. *)
let rec togli x l = match l with (*toglie dalla lista x, se x non c'è non toglie nulla e restituisce la lista completa *)
    []-> []
    |y::rest -> if(x=y) then rest 
                else y:: togli x rest

let rec ramo_da_lista (Tr(x,tlst)) l k = match tlst with
    []-> if (x=k && List.mem x l) then [x]  (*x foglia *)
         else failwith "non è questo ramo"
    |t::rest -> if (List.mem x l) then x:: from_list (togli x l) k tlst 
                else from_list l k tlst 
        and from_list lista k = function
            [] -> failwith "niente"
            |t::rest -> try ramo_da_lista t lista k
                        with _-> from_list lista k rest

(*9. (Dal compito d’esame di settembre 2010).
Scrivere una funzione ramo_di_primi: int ntree -> int che, applicata
a un albero n-ario di interi, riporti, se esiste, una foglia n dell’albero tale
che il ramo dell’albero dalla radice a n sia costituito da tutti numeri primi *)
let primo x = 
    let rec aux x i =
        if (i<x) then ((x mod i)<> 0) && (aux x (i+1))
        else true 
    in aux x 2

let rec ramo_di_primi (Tr(x,tlst)) = match tlst with
    []-> if(primo x) then x else failwith "non c'è" (*x foglia *)
    |_ -> if(primo x) then from_list tlst
                else failwith "na"
                and from_list = function
                    []-> failwith "fine"
                    |t::rest -> try ramo_di_primi t 
                                with _-> from_list rest


(*10. (Dal compito d’esame di giugno 2011, adattato agli alberi n-ari).
Definire una funzione path_non_pred: (’a -> bool) -> ’a ntree -> ’a list, 
che, applicata a un predicato p: ’a -> bool e a un albero
t: ’a ntree, riporti, se esiste, un cammino dalla radice a una foglia di
t che non contenga alcun nodo che soddisfa p. La funzione solleverà
un’eccezione se un tale cammino non esiste. *)
let rec path_non_pred p Tr(x,tlist) = match tlist with
    []-> if(p x) then failwith "na" else [x]
    |_-> if(p x) then failwith "na" else x::from_list p tlist 
        and from_list p lista = match lista with
            []-> []
            |t::rest -> try path_non_pred p t 
                        with _-> from_list p rest


(*11. (Dal compito d’esame di settembre 2011, adattato agli alberi n-ari).
Scrivere un predicato same_structure: ’a ntree -> ’b ntree -> bool
che determini se due alberi n-ari hanno la stessa struttura (cioè se essi sono
uguali quando si ignorano le rispettive etichette). *)

let same_structure Tr(x,t1list) Tr(y,t2list) = (*se ogni volta la lista dei fratelli è lunga uguale allora sono uguali*)
    (List.length t1list = List.length t2list) && (from_list t1list t2list)
            and from_list lista1 lista2= 
            if lista1=[] && lista2=[] then true
            else if ((lista1<>[])&&(lista2<>[])) then (same_structure List.hd lista1 List.hd lista2) && (from_list List.tl lista1 List.tl lista2)
                else if (((lista1=[])&&(lista2<>[]))||((lista1<>[])&&(lista2=[]))) then false 


(*12. (Dal compito d’esame di luglio 2009, adattato agli alberi n-ari).
Si considerino le seguenti dichiarazioni di tipo, per la rappresentazione di
colori e associazioni di colori:
type col = Rosso | Giallo | Verde | Blu
type ’a col_assoc = (col * ’a list) list  [(Rosso,[1;2]);... ]
Scrivere un programma con una funzione
ramo_colorato: ’a -> ’a col_assoc -> ’a ntree -> ’a list, che, dato
un valore x, un’associazione di colori e un albero n-ario, riporti – se esiste
– un ramo a colori alterni, dalla radice dell’albero a una foglia etichettata
da x. Se un tale ramo non esiste, solleverà un’eccezione (si veda l’esercizio
14 del gruppo 8 che propone lo stesso problema per gli alberi binari). *)
type col = Rosso | Giallo | Verde | Blu
type ’a col_assoc = (col * ’a list) list

let rec colore colist nodo = match colist with
    []-> failwith "impossibile"
    |(a,b)::rest-> if (List.mem nodo b) then a else colore rest nodo 

let rec ramo_colorato x colist (Tr(n,tlst)) = match tlst with
    []-> if (n=x) then [n] else failwith "na"                      
    |t::rest-> if (colore t colist <> colore n colist) then n:: try ramo_colorato x colist t
                                                                with _-> from_list x lista
                else failwith "na"
    and from_list x lista = match lista with
        []-> failwith "na"
        |t::rest -> try (ramo_colorato x colist t)  
                    with _-> (from_list x rest)







