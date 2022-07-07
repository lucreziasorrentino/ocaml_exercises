type color = Rosso | Verde | Neutro

let colore cols x =
    try List.assoc x cols
    with _ -> Neutro

let rec conta_colori  cols = function
 [] -> [(Rosso,0);(Verde,0);(Neutro,0)]
 | x::rest -> let tmp = 
            try (List.assoc x cols)
            with _ -> Neutro
            in 
            let [(Rosso,r);(Verde,v);(Neutro,n)] = conta cols rest in
            match tmp with
                Rosso -> [(Rosso,r+1);(Verde,v);(Neutro,n)]
                |Verde -> [(Rosso,r);(Verde,v+1);(Neutro,n)]
                |Neutro -> [(Rosso,r);(Verde,v);(Neutro,n+1)]

let (y,z) = List.hd cols
in if x <> y
    then ...
    else z

let rec trova cols x =
if(cols=[]) then Neutro
else if(x <> fst(List.hd cols)) then trova (List.tl cols) x
else snd (List.hd cols)

let conta_colori_it cols lista=
let rec loop lista r v n=           (*dove loop: 'a list->int->int->int->(int x color)list*)
    match lista with 
    [] -> [(Rosso,r); (Verde,v);(Neutro,n)]
    |x::rest -> if(Rosso =trova cols x) then loop rec (r+1) v n
                else if(Verde= trova cols x) then loop rec r (v+1) n
                else loop rest r v (n+1)
in loop lista () () ()


let conta_colori_it_alternativa cols lista=
let rec loop lista r v n=
    match lista with 
    [] -> [(Rosso,r); (Verde,v);(Neutro,n)]
    |x::rest -> match trova cols x with 
        Rosso->loop rest (r+1) v n 
        | Verde->loop rest r (v+1)n 
        | _-> loop rest r v (n+1)
in loop lista 