(*Definire una funzione ultime_cifre: int -> int * int che riporti il
valore intero delle due ultime cifre di un int. Ad esempio:
ultime_cifre 245 = (4,5)
ultime_cifre 5 = (0,5).
Se il numero è negativo, il segno va ignorato. Ad esempio
ultime_cifre (-245) = (4,5)
ultime_cifre (-5) = (0,5).
La funzione non deve mai sollevare eccezioni, ma riportare sempre una
coppia di interi.
Si cerchi di fornire una soluzione semplice, che operi direttamente sul
numero, anziché passare per la sua rappresentazione come stringa. Si
ricordi che n/m e’ il risultato della divisione intera, n mod m il modulo, e
abs n il valore assoluto.*)
let ultime_cifre n = 
    let x= abs n 
    in ((x mod 100) / 10, (x mod 10));; 

(* Una cifra è bella se è 0, 3, 7; un numero è bello se la sua ultima cifra è bella
e la penultima (se esiste) non lo è. Quindi in particolare le cifre belle sono
numeri belli. Definire un predicato bello: int -> bool che determini
se un numero è bello. La funzione non deve mai sollevare eccezioni, ma
riportare sempre un bool. *)
let rec cifra_bella n =
    if n > -10 && n < 10 then match abs n with
        0|3|7 -> true
        |_-> false
    else let (penultima,ultima) = ultime_cifre n
    in cifra_bella ultima && not (cifra_bella penultima);;

(* (Esercizio 8 pag 45 del libro di testo) Scrivere una funzione data: int *
string -> bool, che, applicata a una coppia (d,m), dove d è un intero e
m una stringa, determini se la coppia rappresenta una data corretta, assumendo 
che l’anno non sia bisestile. Si assume che i mesi siano rappresentati
da stringhe con caratteri minuscoli ("gennaio", "febbraio",. . . ).
La funzione non deve mai sollevare eccezioni, ma riportare sempre un
bool. *)
let data (d,m)= d>0 && match m with
    "gennaio"|"marzo"|"maggio"|"giugno"|"agosto"|"ottobre"|"dicembre" -> d<=31 
    |"febbraio"-> d<=28
    |"novembre" | "aprile" |"giugno" | "settembre"-> d<=30
    |_-> false;;
    