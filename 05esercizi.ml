(*let rec combine lista1 lista2=
 match (lista1,lista2) with
 ([],[]) -> []
 |([x::rest1],[y::rest2]) -> (x,y)::combine rest1 rest2
 |_ -> failwith "lunghezze diverse"*)

let rec split = function
    [] -> ([],[])
  | (x,y)::rest ->
      let (restx,resty) = split rest
      in (x::restx,y::resty)

let rec cancella k= function
[]->[]
|(x,y)::rest -> if x=k then cancella k rest
                else (x,y)::cancella k rest

let rec intpairsbagliato k= match k with
0 -> []
|1 -> [(1,1)]
|x-> (intpairsbagliato (x-1)) @ [((x-1),x)]

let explode s=
 let rec aux n=
  let lunghezza= String.length s in
  if n>=lunghezza then []
  else s.[n]::aux (n+1)
 in aux 0
(*
let implode= function
 [] -> ""
 |x::rest -> (String.make 1 x)^(implode rest)*)


(*4. Scrivere una funzione intpairs: int -> (int*int) list che, applicata a un
intero positivo n, riporti una lista di tutte le coppie di interi (x,y) con x e y
compresi tra 1 e n.
Ad esempio, intpairs 3 riporterà la lista [(1, 1); (1, 2); (1, 3); (2,
1); (2, 2); (2, 3); (3, 1); (3, 2); (3, 3)] (o una sua permutazione).
Suggerimento: risolvere il seguente sottoproblema: dato un elemento y e una
lista [x1;x2;...;xn] (che poi sarà la lista di tutti i numeri compresi tra 1 e
n), costruire la lista [(y,x1);(y,x2);....;(y,xn)]. #use "05esercizi.ml";;*)
let listacompresi x = 
  let rec aux x i =
    if (x<i) then []
    else i:: aux x (i+1)
  in aux x 1;;

let rec f i lst = match lst with (*dovrò passare a i tutti i valori da 1 a n *)
   []-> []
   |x::rest-> (i, x) :: f i rest;;

let intpairs n = (*passa a f n volte f i lst con i uguale a tutti i valori compresi da 1 a n *)
  let rec faux n count = 
    if (count<n) then (f (count+1) (listacompresi n)) :: faux n (count+1)
    else []
  in faux n 0;;
 

(*5. Definire una funzione trips: ’a list -> (’a * ’a * ’a) list che, applicata a una 
lista lst, riporti la lista di tutte le triple adiacenti di elementi di lst
(la lista vuota se lst ha meno di 3 elementi).
Ad esempio trips [1;2;3;4;5] = [(1, 2, 3); (2, 3, 4); (3, 4, 5)] (o
una sua permutazione). #use "05esercizi.ml";; *)
let rec trips lst = match lst with
  x::y::z::rest -> (x,y,z):: trips (y::z::rest)
  |_->[]



(*6. Per “sottolista” di una lista L si intende una “sottosequenza” di L, cioè una lista
contenente elementi che occorrono consecutivi nella lista L, nello stesso ordine.
Ad esempio [1;2;3] è una sottolista di [0;1;2;3;4] ma non di [0;1;2;5;4;3].
Formalmente, una lista L1 è una sottolista di L se esistono liste PRIMA e DOPO
(eventualmente vuote) tali che L = PRIMA @ L1 @ DOPO.
Definire una funzione choose: int -> ’a list -> ’a list list che, applicata a un intero 
positivo k e una lista L, riporti una lista contenente tutte le
sottoliste di L di lunghezza k.
Ad esempio, choose 3 [1;2;3;4;5] = [[1; 2; 3]; [2; 3; 4]; [3; 4; 5]]
(o una sua permutazione).
Suggerimento: utilizzare la funzione take definita a lezione. #use "05esercizi.ml";;*)
let rec auxchoose k lista = match lista with (*mette in una lista k elementi di lista *)
  []-> []
  |x::rest -> if (k>0) then x:: auxchoose (k-1) rest
              else [];; (*oppure auxchoose k rest;;*)

let rec choose k lst = match lst with (*uso auxchoose scorrendo lst avanti di 1 elemento *)
  []-> []
  |x::rest -> if (List.length lst >= k) then [auxchoose k lst] @ choose k rest 
              else [];;

(*7. Definire una funzione strike_ball: ’a list -> ’a list -> (int * int) che,
applicata a due liste, test e guess, che si assumono della stessa lunghezza, 
riporti una coppia (strike,ball) dove strike è il numero di elementi di test
che occorrono anche in guess, ma in diversa posizione, e ball è il numero di
elementi di test che occorrono in guess nella stessa posizione in cui sono in
test.
Ad esempio, strike_ball [1;2;3;4;5;6] [5;6;3;4;2;10] = (3, 2): ci sono
3 elementi “fuori posto” (2,5,6) e 2 nella stessa posizione (3 e 4).
Suggerimento: scandire contemporaneamente le due liste. #use "05esercizi.ml";;*)
let rec ball test guess = match test with
  []-> 0
  |x::rest-> if (x= (List.hd guess)) then 1+ ball rest (List.tl guess) (*se x=primo elemento guess allora ball incrementato e vado avanti con entrambe*)
              else ball rest (List.tl guess);;

let rec esiste n lst = match lst with
  []-> false
  |x::rest-> if n=x then true else esiste n rest;; 

let rec incomune test guess = match test with (*strike = tutti elementi in comune - ball *)
  []-> 0 (*restituisce il numero di tutti gli elementi in comune *)
  |x::rest -> if (esiste x guess) then 1+ incomune rest guess
              else incomune rest guess;;

let strike_ball test guess = (((incomune test guess)-(ball test guess)), ball test guess);;