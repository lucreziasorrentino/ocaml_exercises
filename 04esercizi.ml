let rec reverse lst = match lst with
[]-> []
|x::rest-> reverse rest @ [x]


let rec nth n lst= match lst with
[] -> failwith "errore"
|x::rest-> if n=0 then x (*oppure List.hd rest*)
            else nth (n-1) rest



let rec remove x lst= match lst with
[]-> []
|y::rest -> if x=y then remove x rest
            else y :: remove x rest

let rec copy n x= match n with
0-> []
|_-> x::copy (n-1) x


let rec length= function
[]->0
|_::rest-> 1+length rest

let length_it lst = 
let rec aux x= function
    []->x
    |_::rest-> aux (x+1) rest
in aux 0 lst

let rec nondec= function
[]->true
|x::y::rest-> if x<y then nondec (y::rest) else false


let rec max m = function
[]-> m
|x::y::rest-> if y>x then max y rest 
              else max x rest

(*let min_dei_max lst= 
 let rec listaMassimi lista= match lista with
    []-> []
   |_::rest-> max List.hd lista listaMassimi rest
 in listaMassimi lst
 let minimo= lista*)


(*Una funzione copy: int -> ’a -> ’a list tale che copy n x riporti la lista di
 lunghezza n i cui elementi sono tutti uguali a x.
Determinare il valore e il tipo di copy 3 (copy 2 8)
il tipo è lista di lista : int -> 'a -> 'a list
                          int -> (int list) -> (int list) list
il risultato è [[8;8]; [8;8]; [8;8]]. *)
let rec copy n x = match n with
  0 -> []
  |_-> x :: copy (n-1) x

(*Un predicato nondec: int list -> bool che, applicato a una lista
lst, riporti true se gli elementi di lst sono in ordine non decrescente,
false altrimenti.
Ad esempio, nondec [1;2;3;4] = true, e nondec [1;2;4;3] = false. *)
let rec nondec lst = match lst with
   []|[_] -> true
   |x::rest ->  if (x >= (List.hd rest)) then false
                 else nondec rest

(* Una funzione pairwith: ’a -> ’b list -> (’a * ’b) list che,
applicata a un valore y e una lista xs = [x1;x2;...;xn], riporti la
lista [(y,x1);(y,x2);....;(y,xn)]. ese. pirwith 2 [x1;x2;x3] -> [(2,x1); (2,x2); (2;x3)]*)
let rec pairwith y lst = match lst with
  [] -> []
  |[x] -> [(y,x)] (*anche senza va bene *)
  |x::rest -> (y,x) :: pairwith y rest

(*Una funzione enumera: ’a list -> (int * ’a) list che, applicata a una lista 
lst=[x0;x1;x2;...;xk], riporti la lista di coppie [(0,x0);(1,x1);(2,x2);...;(k,xk)]. *)
let enumera lst =
  let rec aux lst i = match lst with
    []-> []
    |x::rest -> (i,x):: aux rest (i+1)
    in aux lst 0


(*Una funzione position: ’a -> ’a list -> int tale che position
x lst riporti la posizione della prima occorrenza di x in lst (contando a partire da 0). 
Se x non occorre in lst, la funzione solleverà un’eccezione. #use "04esercizi.ml"*)
let position x lst =
  let rec funzi n lista i = match lista with
    []-> failwith "x non è in lista"
    |y::rest -> if y = x then i else funzi n rest (i+1)
    in funzi x lst 0


(* Una funzione alternate: ’a list -> ’a list che, applicata a una
lista lst, riporti la lista contentente tutti e soli gli elementi di lst
che si trovano in posizione dispari. Ricordiamo che, per convenzione,
il primo elemento di una lista si trova in posizione 0, il secondo in
posizione 1, ecc. Quindi, ad esempio, alternate [0;1;20;32;4;5]
= [1;32;5]. *)
let alternate lst = 
  let rec funz lst p = match lst with
      [] -> []
      |x::rest -> if (p mod 2 = 0) then funz rest (p+1) 
                  else x :: funz rest (p+1)
  in funz lst 0

(* split2: ’a list -> ’a list * ’a list, che suddivide una lista
in due liste di lunghezza più o meno uguale, utilizzando le funzioni
take: int -> ’a list -> ’a list, definita a lezione, e drop, dell’esercizio 1d.
Come la funzione split definita a lezione, la split2
si potrebbe utilizzare per implementare il merge sort.
A differenza di split, che mette gli elementi in posizione pari nella
prima lista, quelli in posizione dispari nella seconda, split2 metterà i primi 
elementi da una parte e gli ultimi dall’altra. Ad esempio,
split2 [1;2;3;4;5;6;7] = ([1;2;3], [4;5;6;7]), mentre split
[1;2;3;4;5;6;7] = ([1;3;5;7], [2;4;6]). #use "04esercizi.ml"*)
let rec togli lst x i= match lst with
[]->[]
|y::rest-> if i=x then y::rest
            else (togli rest x (i+1));;

let rec metti lst x i= match lst with (* x= lunghezza nuova lista e i=parte da 0 *)
[]->[]
|y::rest-> if i<x then y:: metti rest x (i+1) 
            else [];;

let split2 lst = 
  (metti lst (List.length lst/2) 0  , togli lst (List.length lst/2) 0);;
