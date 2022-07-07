(*FILE USATO PER TESTARE*)


(*type col = Rosso | Giallo | Verde | Blu
type 'a col_assoc = (col * 'a list) list
type 'a ntree = Tr of 'a* 'a ntree list
let leaf x = Tr(x,[])
let lst = [(Rosso,[1;4;7;10]); (Giallo,[3;8;11]); (Verde,[0;2;5;6;13]); (Blu,[9;12;14;15])]
let t = Tr(1,[Tr(2,[Tr(3,[leaf 4; leaf 5]); Tr(6,[leaf 7]); leaf 8]); leaf 9; Tr(10,[Tr(11,[leaf 12; leaf 13; leaf 14]); leaf 15; Tr(16,[leaf 17; Tr(18,[leaf 19; leaf 20])])])])


let rec postorder (Tr(x,tlst)) = match tlst with
    []-> [x]
    |_ -> from_list tlst @ [x]
                and from_list = function
                    [] -> []
                    |t::rest-> (postorder t @ from_list rest)

let rec inorder (Tr(x,tlst)) = match tlst with
    []-> [x]
    |t::rest -> inorder t @ ([x]  @ from_list rest)
                and from_list = function
                    [] -> []
                    |t::rest-> (inorder t @ from_list rest)*)

let colori_alterni grafo col start goal =
    let rec colori_node visited primo nodo = match nodo with
        []-> failwith "na"
        |n::rest-> if (List.mem nodo visited) then failwith "na"
                    else if (colore n col <> colore primo col)&&(n <> goal) then n:: colori_node (n::visited) nodo (successori n)
                            else if ((n = goal)&&(colore n col <> colore primo col)) then [n]
                                 else colori_list (n::visited) primo (rest)
            and colori_list visitati primo = function
                []-> failwith "na"
                |l::rest -> try-> colori_node visitati primo l
                            with _ -> colori_list visitati primo rest
    in start :: colori_node [] [start] (successori start)


type 'a tree =
 Empty
| Tr of int * 'a tree * 'a tree;;
let tb= (Tr(1, Tr(2, Tr(4,Empty,Empty), Empty),Tr(3, Tr(5, Tr(6,Empty,Empty), Tr(7,Empty,Empty)),Empty)))
let foglie_costi t = match t with
    Empty-> failwith "qui non ci finisci mai"
    |Tr(x, Empty, Empty)-> [(x,x)] (*foglia*)
    |Tr(x, t, Empty) | Tr(x, Empty, t)  -> let (a,b) = foglie_costi t
                                            in (a, b+x)
    |Tr(x,t1,t2)-> let (a1,b1) = foglie_costi t1 in
                let (a2,b2) = foglie_costi t2 in 
                (a1, x+b1)@(a2, x+b2) 

(* #use "test.ml";; primo 5;; *)