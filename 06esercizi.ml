let rec find x= function
 []-> failwith "x non presente"
 |y::rest -> if x=y then ([],rest)
            else let (prima,dopo)= find x rest
            in (y::prima,dopo)

let spezza x lst=
 find x (snd (find x lst))

let esmpio lst=
List.flatten lst

let cons x lst= x::lst

(*ESERCIZI DA GENNAIO *)

(*(a) find: (’a -> bool) -> ’a list -> ’a, tale che find p lst riporti
il primo elemento di lst che soddisfa il predicato p. La funzione solleva un’eccezione 
se nessun elemento della lista soddisfa p. (Notare che il
modulo List contiene una funzione con questo nome, ma qui si chiede di
ridefinirla per esercizio).
Utilizzare find per definire una funzione find_applicata: int list ->int che, 
applicata a una lista di interi, riporti il primo elemento della lista
il cui quadrato sia minore di 30  #use "06esercizi.ml";; *)
let auxfindapplicata x = ((x*x) < 30);;

let rec find p lst = match lst with
    []-> failwith "na"
    |x::rest -> if (p x) then x else find p rest;;

let find_applicata lst = find auxfindapplicata lst;;

let find_applicata_prof lst = find (function x -> (x*x) < 30) lst;;

(*(b) takewhile: (’a -> bool) -> ’a list -> ’a list, tale che takewhile
p lst riporti la più lunga parte iniziale di lst costituita tutta da elementi
che soddisfano il predicato p. Gli elementi del risultato devono occorrere
nello stesso ordine in cui occorrono nell’argomento.
Ad esempio, takewhile (function n -> n mod 2 = 0) [0;2;4;6;7;8;9;10] = [0; 2; 4; 6]. 
#use "06esercizi.ml";;*)

(*FUNZIONE INUTILE PER QUESTO ESERCIZIO MA LA LASCIO PERCHE SEMBRA BELLA
riparti dall'indice dall'elemento che non rispetta p ese. riparti 3 [0;1;2;3;4;5;6] 0 = [3;4;5;6]*)
let rec riparti i lst count = match lst with
    []->[]
    |x::rest -> if (count < i) then riparti i rest (count +1)
            else x::rest;;

let rec takewhile p lst = match lst with (*metto in una lista tutti gli elementi che soddisfano e conto la lunghezza, poi confronto la lunghezza con quella dopo e vedo quale è maggiore *)
    []-> []
    |x::rest -> if (p x) then x:: takewhile p rest
                else [];;

(*(c) dropwhile: (’a -> bool) -> ’a list -> ’a list, tale che dropwhile
p lst riporti la lista che si ottiene eliminando i primi elementi di lst, fino
a che soddisfano il predicato p. Gli elementi del risultato devono occorrere
nello stesso ordine in cui occorrono nell’argomento.
Ad esempio, dropwhile (function n -> n mod 2 = 0) [0;2;4;6;7;8;9;10] = [7; 8; 9; 10]. #use "06esercizi.ml";; *)
let rec dropwhile p lst = match lst with
    []-> []
    |x::rest -> if p x then dropwhile p rest
                else x::rest;;

(*(d) partition: (’a -> ’bool) -> ’a list -> (’a list * ’a list), tale
che partition p lst = (yes,no), dove yes contiene tutti gli elementi di
lst che soddisfano il predicato p, e no quelli che non lo soddisfano. Gli
elementi delle due liste yes e no possono essere in qualsiasi ordine.
Ad esempio, il valore di partition (function n -> n mod 2 = 0) [0;2;4;6;7;8;9;10] 
può essere ([10; 8; 6; 4; 2; 0], [9; 7]). #use "06esercizi.ml";; *)

let rec partition p lst = match lst with 
    []-> ([],[])
    |x::rest -> let (yes,no) = partition p rest
                in if (p x) then (x::yes, no)
                    else (yes, x::no);;

(*(e) pairwith: ’a -> ’b list -> (’a * ’b) list tale che, 
pairwith y [x1;x2;...;xn] = [(y,x1);(y,x2);....; (y,xn)]. Utilizzare la funzione List.map #use "06esercizi.ml";;*)
let auxpair y x = (y,x);;
let pairwith y lst = List.map (auxpair y) lst ;;

(*(f) verifica_matrice: int -> int list list -> bool, che, dato un intero n e una matrice di interi, 
rappresentata mediante liste di liste, riporta true se la matrice contiene almeno una riga i cui elementi
siano tutti minori di n, false altrimenti. Utilizzare le funzioni List.exists e List.for_all. 
#use "06esercizi.ml";; *)
let verifica_matrice n matr =
    List.exists (function row -> List.for_all (function x -> x<n) row) matr ;;

(*(g) setdiff: ’a list -> ’a list -> ’a list, la differenza insiemistica, utilizzando la funzione List.filter. *)
let setdiff lst1 lst2 = (*rimangono in lst1 gli elementi che soddisfano la non presenza in list 2*)
    List.filter (function x -> not (List.exists (function n -> n=x) lst2)) lst1 ;;
let setdiff_prof lst1 lst2 =
    List.filter (function x -> not (List.mem x lst2)) lst1;;

(*(h) subset: ’a list -> ’a list -> bool, tale che subset set1 set2 = true 
se set1 rappresenta un sottoinsieme di set2. Utilizzare la funzione List.for_all.
#use "06esercizi.ml";; *)
let subset set1 set2 = 
    List.for_all (function x-> List.mem x set2) set1;;

(*(i) duplica: int list -> int list, che raddoppia tutti gli elementi di
una lista di interi, usando la funzione List.map. Ad esempio, duplica [0;1;2;3;4] = [0; 2; 4; 6; 8] 
#use "06esercizi.ml";;*)
let duplica lst = 
    List.map (function x-> 2*x) lst;;

(*(j) mapcons: (’a * ’b list) list -> ’b -> (’a * ’b list) list,
che, data una lista di coppie L e un elemento x: ’b, riporta la lista che
si ottiene inserendo x in testa a ogni secondo elemento delle coppie in L.
Utilizzare la funzione List.map.
Ad esempio, applicata alla lista [(’A’,[1;2]); (’B’,[3;4;5]); (’C’,[])]
e al valore 0, la funzione riporta [(’A’,[0;1;2]); (’B’,[0;3;4;5]); (’C’,[0])]. 
#use "06esercizi.ml";;*)
let mapcons lst x = 
    List.map (function (a, lstmini) -> (a, x::lstmini)) lst;;

(*(k) tutte_liste_con: int -> ’a -> ’a -> ’a list list, che, dato un
intero non negativo n e due valori (dello stesso tipo) x e y, riporta una lista
contenente tutte le possibili liste di lunghezza n contenenti soltanto i due
valori x e y.
Ad esempio, per n=3, x=0 e y=1 si avra‘ la lista seguente (o una sua permutazione):
[[0; 0; 0]; [0; 0; 1]; [0; 1; 0]; [0; 1; 1]; [1; 0; 0]; [1; 0; 1]; [1; 1; 0]; [1; 1; 1]] 
#use "06esercizi.ml";;*)
let rec tutte_liste_con n x y =  
    if n=0 then [[]]
    else (List.map (function a -> x::a) (tutte_liste_con (n-1) x y)) @ (List.map (function a -> y::a) (tutte_liste_con (n-1) x y)) ;;

(*(l) interleave: ’a -> ’a list -> ’a list list, tale che interleave
x lst riporti una lista con tutte le liste che si ottengono inserendo x in qualsiasi posizione in lst. 
Utilizzare la funzione List.map.
Ad esempio, interleave 10 [0;1;2] = [[10; 0; 1; 2]; [0; 10; 1; 2]; [0; 1; 10; 2]; [0; 1; 2; 10]]. 
#use "06esercizi.ml";;*)

let rec interleave x lst = match lst with
    []-> [[x]]
    |y::rest-> (x::y::rest) :: (List.map (function n-> y::n) (interleave x rest));;

(*spiegazione interleave: 
interleave 10 [0;1;2]:  [10;0;1;2] :: [0:: ogni elemento di interleave 10 [1;2]]| cioè: [10;0;1;2] :: [0:: ogni elemento di [[10;1;2]; [1;10;2]; [1;2;10]]  | cioè: [10;0;1;2] :: [[0;10;1;2]; [0;1;10;2]; [0;1;2;10]]  -> [[10;0;1;2]; [0;10;1;2]; [0;1;10;2]; [0;1;2;10]]
interleave 10 [1;2]:    [10;1;2]   :: [1:: ogni elemento di interleave 10 [2]]  | cioè: [10;1;2]   :: [1:: ogni elemento di [[10;2]; [2;10]]]               | cioè: [10;1;2] :: [[1;10;2]; [1;2;10]]                    -> [[10;1;2]; [1;10;2]; [1;2;10]]
interleave 10 [2]:      [10;2]     :: [2:: ogni elemento di interleave 10 [2]]  | cioè: [10;2]     :: [2:: ogni elemento di  [[10]]]                        | cioè: [10;2] :: [2;10]                                    -> [[10;2]; [2;10]]
interleave 10 []:       [[10]]
*)


(*(m) permut: ’a list -> ’a list list, tale che permut lst riporti una
lista con tutte le permutazioni di lst (in qualsiasi ordine). 
permutt [1;2;3] = [[1;2;3];[1;3;2];[2;1;3];[2;3;1];[3;1;2];[3;2;1]]
#use "06esercizi.ml";;*)

let rec permutt lst = match lst with
    []-> [[]] 
    |x::rest-> List.flatten (List.map (interleave x) (permutt rest)) ;;