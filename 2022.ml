(*esonero alberi:
restituisci fratelli di un nodo x in una lista, senza che nella lista ci siano ripetizioni e senza x *)

let rec fratelli n Tr(x,tlst) = match tlst with
    []-> failwith "no"
    |t::rest -> if (t=x) then rest
                else from_list 
                    and from_list n lista = match lista with
                        []-> failwith "no"
                        |t::rest -> try fratelli n t  
                                    with _-> from_list rest 


(*restituisci tripla (a,b,c) in cui 
a= radice
b= cammino
c= foglia
in cui tutti i nodi fino alla foglia soddisfano p *)

let tripla p (Tr(x,tlst)) =
    let rec cammino p (Tr(x,tlst)) = match tlst with
        [] -> [x]
        |t::rest -> if (p x) then x:: from_list p tlst
                    else failwith "na"
                        and from_list p list= match list with
                            []-> failwith "na"
                            |t::rest -> try tripla p t
                                        with _-> from_list p rest 
    in let lista = cammino p (Tr(x,tlst)) in (x, lista, List.nth ((List.length lista)-1) lista)


(*grafo orientato con alcuni nodi che hanno un costo 
i costi sono in una lista
avendo un intero x devi arrivare al nodo *)
