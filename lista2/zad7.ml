
type 'a tree =
  | Leaf
  | Node of 'a tree * 'a * 'a tree * int

let make xt n yt = 
  match xt, yt with
  | Leaf, Leaf -> Node(Leaf, n, Leaf, 0)
  | xt, Leaf -> Node(xt, n, Leaf, 0)
  | Leaf, yt -> Node(yt, n, Leaf, 0)
  | Node(l1, n1, r1, h1), Node(l2, n2, r2, h2) -> if h1 > h2
    then Node(xt, n, yt, h2 + 1)
    else Node(yt, n, xt, h1 + 1) 


let cotomabyc x = Node(Leaf, x, Leaf, 0);;

let rec merge xt yt =
  match xt, yt with
  | Leaf, Leaf -> Leaf
  | xt, Leaf -> xt
  | Leaf, yt -> yt
  | Node(l1, n1, r1, h1), Node(l2, n2, r2, h2) -> if n1 < n2
    then make (merge yt r1) n1 l1
    else make (merge xt r2) n2 l2
  

let insert x xt = merge (cotomabyc x) xt

let pop xt = 
  match xt with
  | Leaf -> failwith "dupa"
  | Node(l, n, r, h) -> (n, merge l r)


let rec list_of_heap xt = 
  match xt with
  | Leaf -> []
  | xt -> let (n, xt) = pop xt in n :: (list_of_heap xt)


let heapsort xs = list_of_heap (List.fold_left (fun acc x -> insert x acc) Leaf xs)



