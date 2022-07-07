(* Rappresentiamo le ore della giornata mediante coppie (h,m): int * int,
dove h è compreso tra 0 e 23, inclusi (le ore) e m è compreso tra 0 e 59,
inclusi (i minuti). Scrivere un programma con una funzione
somma_ore: (int * int) -> (int * int) -> int * int,
che calcoli la somma di due ore così rappresentate.
Ad esempio:
somma_ore (3,15) (4,20) = (7,35)
somma_ore (3,45) (4,20) = (8,5)
somma_ore (23,45)(0,20) = (0,5)
Se uno dei due argomenti non è la rappresentazione corretta di un’ora, la
funzione solleverà un’eccezione.
Ricordarsi, anche in questo esercizio, le fun *)
let somma_ore (a,b) (c,d) = 
    if (a < 0 || a > 24) || (c < 0 || c > 24) || (b < 0 || b > 60) || (d < 0 || d > 60) then failwith "errore"
    else (((a + c) + ((b + d) /60) ) mod 24, (b + d) mod 60);;

    (* #use "03esercizi.ml";;
     *)

(*sumbetween: int -> int -> int, tale che sumbetween n m = 
somma degli interi compresi tra n e m (estremi inclusi  1 3   1 2 3 = 3+3= 6*)
let rec sumbetween n m = 
    if n < m then n + sumbetween (n + 1) m
    else n;;


(*sumto: int -> int, tale che sumto n = somma degli interi compresi
tra 0 e n (incluso), assumendo n ≥ 0 *)
let sumto n = 
    sumbetween 0 n;;

let rec sumto_ric n =
    if n > 0 then n + sumto_ric n-1
    else 0;;

(*power: int -> int -> int, tale che power n k = k-esima potenza
di n (assumendo n, k ≥ 0). *)
let rec power n k = 
    if k>0 then n * power n (k - 1)
    else 1;;

(*fib: int -> int, tale che fib n = n-esimo numero di Fibonacci
(assumendo n ≥ 0).
La sequenza dei numeri di Fibonacci è così definita:
fib 0 = 0,
fib 1 = 1,
fib n = fib (n-1) + fib(n-2) *)
let rec fib n = match n with
    0 -> 0
    |1 -> 1 
    |_-> (fib (n-1)) + (fib(n-2));;

(*maxstring: string -> char, tale che maxstring s = massimo carattere in s (secondo il codice ASCII).
Ad esempio: maxstring "antonio"= 't'. Se l’argomento è la stringa vuota, la funzione solleverà un’eccezion *)
(*uso code che mi da il valore ASCII del char *)
let rec conta s l i m c =
    if i >= l then c
    else if Char.code(s.[i]) > m then conta s l (i+1) (Char.code(s.[i])) (s.[i])
        else  conta s l (i+1) m c


let maxstring s =
    if s = "" then failwith "nooooooooooooooo"
    else conta s (String.length s) (1) (Char.code(s.[0])) (s.[0])