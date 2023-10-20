

let rec merge cmp xs ys = 
  match (xs, ys) with
  | ([], []) -> []
  | (xs , []) -> xs
  | ([], ys) -> ys
  | (x :: xs, y :: ys) -> if cmp x y
                          then x :: (merge cmp xs (y :: ys))
                          else y :: (merge cmp (x :: xs) ys) 


let merge_tail cmp xl yl = 
  let rec inner m xs ys = 
    match (xs, ys) with
    | ([], []) -> m
    | (x :: xs, []) -> inner (x :: m) xs []
    | ([], y :: ys) -> inner (y :: m) [] ys
    | (x :: xs, y :: ys) -> if cmp x y
                            then inner (x :: m) xs (y :: ys)
                            else inner (y :: m) (x :: xs) ys
  in inner [] xl yl


let merge_tail2 cmp xl yl = 
  let rec inner xs ys f = 
    match (xs, ys) with
    | ([], []) -> f []
    | (x :: xs, []) -> inner xs [] (fun c -> f (x :: c))
    | ([], y :: ys) -> inner [] ys (fun c -> f (y :: c))
    | (x :: xs, y :: ys) -> if cmp x y
                            then inner xs (y :: ys) (fun c -> f (x :: c))
                            else inner (x :: xs) ys (fun c -> f (y :: c))
  in inner xl yl (fun x -> x)   
  
let halve xl = 
  let rec inner zs xs ys =
    match (zs, xs, ys) with
    | ([], xs, ys) -> (xs, ys)
    | ((z :: s :: zs), xs, ys) -> inner zs (z :: xs) (s :: ys) 
    | ([z], xs, ys) -> (z :: xs, ys)
  in inner xl [] []

let rec mergesort cmp zs = 
  match halve zs with
  | ([], []) -> []
  | (xs, []) -> xs
  | (xs, ys) -> merge cmp (mergesort cmp xs) (mergesort cmp ys)


let neg p x y = not (p x y) 

let rec mergesort2' cmp za = 
  match halve za with
  | ([], []) -> []
  | (xs, []) -> xs
  | (xs, ys) -> merge (neg cmp) (mergesort2 cmp xs) (mergesort2 cmp ys)

let mergesort2 cmp = mergesort2' (neg cmp)